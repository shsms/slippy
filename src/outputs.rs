use swayipc::Connection;

use tulisp::{
    intern, lists::plist_get, tulisp_add_func, tulisp_fn, Error, ErrorKind, TulispContext,
    TulispObject,
};

intern! {
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

pub(super) fn register_methods(ctx: &mut TulispContext) {
    tulisp_add_func!(ctx, get_outputs, "get-outputs");
    tulisp_add_func!(ctx, set_output, "set-output");
}

#[tulisp_fn]
fn get_outputs(ctx: &mut TulispContext) -> Result<TulispObject, Error> {
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

    let kw = Keywords::new(ctx);

    for output in outputs {
        results.push(tulisp::lists::plist_from([
            (kw.name.clone(), output.name.into()),
            (kw.make.clone(), output.make.into()),
            (kw.model.clone(), output.model.into()),
            (kw.serial.clone(), output.serial.into()),
            (kw.active.clone(), output.active.into()),
            (
                kw.resolution.clone(),
                output
                    .current_mode
                    .map(|x| format!("{}x{}", x.width, x.height).into())
                    .unwrap_or_default(),
            ),
            (
                kw.current_mode.clone(),
                output
                    .current_mode
                    .map(|x| format!("{}x{}@{}", x.width, x.height, x.refresh as f64 / 1e3).into())
                    .unwrap_or_default(),
            ),
            (
                kw.refresh.clone(),
                output
                    .current_mode
                    .map(|x| { x.refresh as f64 / 1e3 }.into())
                    .unwrap_or_default(),
            ),
            (
                kw.scale.clone(),
                output.scale.map(|x| x.into()).unwrap_or_default(),
            ),
            (kw.pos_x.clone(), (output.rect.x as i64).into()),
            (kw.pos_y.clone(), (output.rect.y as i64).into()),
            (kw.width.clone(), (output.rect.width as i64).into()),
            (kw.height.clone(), (output.rect.height as i64).into()),
            (
                kw.modes.clone(),
                output
                    .modes
                    .into_iter()
                    .map(|x| {
                        tulisp::lists::alist_from([
                            (
                                kw.mode.clone(),
                                format!("{}x{}@{}", x.width, x.height, x.refresh as f64 / 1e3)
                                    .into(),
                            ),
                            (
                                kw.resolution.clone(),
                                format!("{}x{}", x.width, x.height).into(),
                            ),
                            (kw.refresh.clone(), (x.refresh as f64 / 1e3).into()),
                        ])
                    })
                    .collect(),
            ),
        ]))?;
    }
    Ok(results)
}

#[tulisp_fn]
fn set_output(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
    let kw = Keywords::new(ctx);

    let args = rest.clone();

    let tgt_name: Option<String> = plist_get(&args, &kw.name)?.try_into()?;
    let tgt_scale: Option<f64> = plist_get(&args, &kw.scale)?.try_into()?;
    let tgt_resolution: Option<String> = plist_get(&args, &kw.resolution)?.try_into()?;
    let tgt_pos_x: Option<i64> = plist_get(&args, &kw.pos_x)?.try_into()?;
    let tgt_pos_y: Option<i64> = plist_get(&args, &kw.pos_y)?.try_into()?;

    if tgt_name.is_none() {
        return Err(Error::new(
            ErrorKind::MissingArgument,
            "set-output: no output name specified.".into(),
        ));
    }
    if (tgt_pos_x.is_none() && tgt_pos_y.is_some()) || (tgt_pos_x.is_some() && tgt_pos_y.is_none())
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

    get_outputs(ctx)?
        .base_iter()
        .find(|x| {
            plist_get(x, &kw.name)
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
