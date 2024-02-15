use std::rc::Rc;

use swayipc::Connection;

use tulisp::{intern, lists::plist_get, Error, ErrorKind, TulispContext, TulispObject};

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

pub(crate) fn init(ctx: &mut TulispContext) {
    let tm = Rc::new(Output {
        kw: Keywords::new(ctx),
    });
    let copy = tm.clone();
    ctx.add_special_form("get-outputs", move |a, b| copy.get_outputs(a, b));
    ctx.add_special_form("set-output", move |a, b| tm.set_output(a, b));
}

impl Output {
    fn get_outputs(
        &self,
        _ctx: &mut TulispContext,
        _args: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let mut conn = Connection::new().map_err(|e| {
            Error::new(
                ErrorKind::Uninitialized,
                format!("get-outputs: failed to connect to sway: {}", e),
            )
        })?;
        let outputs = conn.get_outputs().map_err(|e| {
            Error::new(
                ErrorKind::Uninitialized,
                format!("get-outputs: failed to get outputs: {}", e),
            )
        })?;
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

    fn set_output(
        &self,
        ctx: &mut TulispContext,
        plist: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let plist = ctx.eval_each(plist)?;

        let tgt_name: Option<String> = plist_get(&plist, &self.kw.name)?.try_into()?;
        let tgt_scale: Option<f64> = plist_get(&plist, &self.kw.scale)?.try_into()?;
        let tgt_resolution: Option<String> = plist_get(&plist, &self.kw.resolution)?.try_into()?;
        let tgt_pos_x: Option<i64> = plist_get(&plist, &self.kw.pos_x)?.try_into()?;
        let tgt_pos_y: Option<i64> = plist_get(&plist, &self.kw.pos_y)?.try_into()?;

        if tgt_name.is_none() {
            return Err(Error::new(
                ErrorKind::MissingArgument,
                "set-output: no output name specified.".into(),
            ));
        }
        if (tgt_pos_x.is_none() && tgt_pos_y.is_some())
            || (tgt_pos_x.is_some() && tgt_pos_y.is_none())
        {
            return Err(Error::new(
                ErrorKind::MissingArgument,
                "set-output: pos-x and pos-y must be specified together.".into(),
            ));
        }
        if tgt_resolution.is_none() && tgt_scale.is_none() && tgt_pos_x.is_none() {
            return Err(Error::new(
                ErrorKind::MissingArgument,
                "set-output: no target parameters specified.".into(),
            ));
        }

        let mut cmd = format!("output {}", tgt_name.as_ref().unwrap());

        if tgt_pos_x.is_some() {
            cmd.push_str(&format!(
                " pos {} {}",
                tgt_pos_x.unwrap(),
                tgt_pos_y.unwrap()
            ));
        }

        if tgt_resolution.is_some() {
            cmd.push_str(&format!(" res {}", tgt_resolution.unwrap()));
        }

        if tgt_scale.is_some() {
            cmd.push_str(&format!(" scale {}", tgt_scale.unwrap()));
        }

        let mut conn = Connection::new().map_err(|e| {
            Error::new(
                ErrorKind::Uninitialized,
                format!("set-output: failed to connect to sway: {}", e),
            )
        })?;

        conn.run_command(&cmd).map_err(|e| {
            Error::new(
                ErrorKind::Uninitialized,
                format!("set-output: failed to run command: {}", e),
            )
        })?;

        self.get_outputs(ctx, &TulispObject::nil())?
            .base_iter()
            .find(|x| {
                plist_get(x, &self.kw.name)
                    .map(|x| tgt_name == x.try_into().ok())
                    .unwrap_or(false)
            })
            .ok_or_else(|| {
                Error::new(
                    ErrorKind::Uninitialized,
                    format!("set-output: failed to find output: {}", tgt_name.unwrap()),
                )
            })
    }
}
