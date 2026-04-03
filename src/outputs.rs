use std::rc::Rc;

use swayipc::Connection;

use tulisp::{AsPlist, Error, Plist, TulispContext, TulispObject, intern, lists::plist_get};

intern! {
    #[derive(Clone)]
    struct Keywords {
        name: ":name",
        make: ":make",
        model: ":model",
        serial: ":serial",
        active: ":active",
        resolution: ":resolution",
        current_mode: ":current-mode",
        transform: ":transform",
        refresh: ":refresh",
        scale: ":scale",
        pos_x: ":pos-x",
        pos_y: ":pos-y",
        width: ":width",
        height: ":height",
        modes: ":modes",
        mode: ":mode",
    }
}

#[derive(Clone)]
struct Output {
    kw: Keywords,
}

AsPlist! {
    struct SetOutputParams {
        name: String,
        resolution: Option<String> {= None},
        transform: Option<String> {= None},
        refresh: Option<f64> {= None},
        scale: Option<f64> {= None},
        pos_x<":pos-x">: Option<i64> {= None},
        pos_y<":pos-y">: Option<i64> {= None},
    }
}

pub(crate) fn init(ctx: &mut TulispContext) {
    let tm = Rc::new(Output {
        kw: Keywords::new(ctx),
    });
    let copy = tm.clone();
    ctx.add_function("get-outputs", move || copy.get_outputs());
    ctx.add_function("set-output", move |p: Plist<SetOutputParams>| {
        tm.set_output(p)
    });
}

impl Output {
    fn get_outputs(&self) -> Result<TulispObject, Error> {
        let mut conn = Connection::new().map_err(|e| {
            Error::os_error(format!("get-outputs: failed to connect to sway: {}", e))
        })?;
        let outputs = conn
            .get_outputs()
            .map_err(|e| Error::os_error(format!("get-outputs: failed to get outputs: {}", e)))?;
        let results = TulispObject::nil();

        for output in outputs {
            results.push(tulisp::lists::plist_from([
                (self.kw.name.clone(), output.name.into()),
                (self.kw.make.clone(), output.make.into()),
                (self.kw.model.clone(), output.model.into()),
                (self.kw.serial.clone(), output.serial.into()),
                (self.kw.active.clone(), output.active.into()),
                (
                    self.kw.resolution.clone(),
                    output
                        .current_mode
                        .map(|x| format!("{}x{}", x.width, x.height).into())
                        .unwrap_or_default(),
                ),
                (
                    self.kw.current_mode.clone(),
                    output
                        .current_mode
                        .map(|x| {
                            format!("{}x{}@{}", x.width, x.height, x.refresh as f64 / 1e3).into()
                        })
                        .unwrap_or_default(),
                ),
                (
                    self.kw.refresh.clone(),
                    output
                        .current_mode
                        .map(|x| { x.refresh as f64 / 1e3 }.into())
                        .unwrap_or_default(),
                ),
                (
                    self.kw.transform.clone(),
                    output.transform.map(|x| x.into()).unwrap_or_default(),
                ),
                (
                    self.kw.scale.clone(),
                    output.scale.map(|x| x.into()).unwrap_or_default(),
                ),
                (self.kw.pos_x.clone(), (output.rect.x as i64).into()),
                (self.kw.pos_y.clone(), (output.rect.y as i64).into()),
                (self.kw.width.clone(), (output.rect.width as i64).into()),
                (self.kw.height.clone(), (output.rect.height as i64).into()),
                (
                    self.kw.modes.clone(),
                    output
                        .modes
                        .into_iter()
                        .map(|x| {
                            tulisp::lists::alist_from([
                                (
                                    self.kw.mode.clone(),
                                    format!("{}x{}@{}", x.width, x.height, x.refresh as f64 / 1e3)
                                        .into(),
                                ),
                                (
                                    self.kw.resolution.clone(),
                                    format!("{}x{}", x.width, x.height).into(),
                                ),
                                (self.kw.refresh.clone(), (x.refresh as f64 / 1e3).into()),
                            ])
                        })
                        .collect(),
                ),
            ]))?;
        }
        Ok(results)
    }

    fn set_output(&self, plist: Plist<SetOutputParams>) -> Result<TulispObject, Error> {
        if (plist.pos_x.is_none() && plist.pos_y.is_some())
            || (plist.pos_x.is_some() && plist.pos_y.is_none())
        {
            return Err(Error::invalid_argument(
                "set-output: pos-x and pos-y must be specified together.",
            ));
        }
        if plist.resolution.is_none() && plist.scale.is_none() && plist.pos_x.is_none() {
            return Err(Error::invalid_argument(
                "set-output: no target parameters specified.",
            ));
        }

        let mut cmd = format!("output {}", plist.name);

        if let Some(ref tgt_transform) = plist.transform {
            if tgt_transform != "normal"
                && tgt_transform != "90"
                && tgt_transform != "180"
                && tgt_transform != "270"
                && tgt_transform != "flipped-90"
                && tgt_transform != "flipped-180"
                && tgt_transform != "flipped-270"
            {
                return Err(Error::invalid_argument(format!(
                    concat!(
                        "set-output: invalid transform: {}. ",
                        "Must be one of: normal, 90, 180, 270, flipped-90, flipped-180, flipped-270."
                    ),
                    tgt_transform
                )));
            }
            cmd.push_str(&format!(" transform {}", tgt_transform));
        }

        match (plist.pos_x, plist.pos_y) {
            (Some(x), Some(y)) => {
                cmd.push_str(&format!(" pos {} {}", x, y));
            }
            (None, None) => {}
            _ => println!(
                "Warning: pos-x and pos-y should be specified together. Ignoring position parameters."
            ),
        }

        if let Some(ref res) = plist.resolution {
            cmd.push_str(&format!(" res {res}"));
        }
        if let Some(ref scale) = plist.scale {
            cmd.push_str(&format!(" scale {scale}"));
        }

        let mut conn = Connection::new().map_err(|e| {
            Error::os_error(format!("set-output: failed to connect to sway: {}", e))
        })?;

        conn.run_command(&cmd)
            .map_err(|e| Error::os_error(format!("set-output: failed to run command: {}", e)))?;

        self.get_outputs()?
            .base_iter()
            .find(|x| {
                plist_get(x, &self.kw.name)
                    .map(|x| {
                        x.try_into()
                            .ok()
                            .is_some_and(|name: String| name == plist.name)
                    })
                    .unwrap_or(false)
            })
            .ok_or_else(|| {
                Error::os_error(format!("set-output: failed to find output: {}", plist.name))
            })
    }
}
