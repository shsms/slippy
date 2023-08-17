use swayipc::Connection;

use tulisp::{
    lists::plist_get, tulisp_add_func, tulisp_fn, Error, ErrorKind, TulispContext, TulispObject,
};

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

    let name = ctx.intern(":name");
    let make = ctx.intern(":make");
    let model = ctx.intern(":model");
    let serial = ctx.intern(":serial");
    let active = ctx.intern(":active");
    let resolution = ctx.intern(":resolution");
    let current_mode = ctx.intern(":current-mode");
    let refresh = ctx.intern(":refresh");
    let scale = ctx.intern(":scale");
    let pos_x = ctx.intern(":pos-x");
    let pos_y = ctx.intern(":pos-y");
    let width = ctx.intern(":width");
    let height = ctx.intern(":height");
    let modes = ctx.intern(":modes");
    let mode = ctx.intern(":mode");

    for output in outputs {
        results.push(tulisp::lists::plist_from([
            (name.clone(), output.name.into()),
            (make.clone(), output.make.into()),
            (model.clone(), output.model.into()),
            (serial.clone(), output.serial.into()),
            (active.clone(), output.active.into()),
            (
                resolution.clone(),
                output
                    .current_mode
                    .map(|x| format!("{}x{}", x.width, x.height).into())
                    .unwrap_or_default(),
            ),
            (
                current_mode.clone(),
                output
                    .current_mode
                    .map(|x| format!("{}x{}@{}", x.width, x.height, x.refresh as f64 / 1e3).into())
                    .unwrap_or_default(),
            ),
            (
                refresh.clone(),
                output
                    .current_mode
                    .map(|x| { x.refresh as f64 / 1e3 }.into())
                    .unwrap_or_default(),
            ),
            (
                scale.clone(),
                output.scale.map(|x| x.into()).unwrap_or_default(),
            ),
            (pos_x.clone(), (output.rect.x as i64).into()),
            (pos_y.clone(), (output.rect.y as i64).into()),
            (width.clone(), (output.rect.width as i64).into()),
            (height.clone(), (output.rect.height as i64).into()),
            (
                modes.clone(),
                output
                    .modes
                    .into_iter()
                    .map(|x| {
                        tulisp::lists::alist_from([
                            (
                                mode.clone(),
                                format!("{}x{}@{}", x.width, x.height, x.refresh as f64 / 1e3)
                                    .into(),
                            ),
                            (
                                resolution.clone(),
                                format!("{}x{}", x.width, x.height).into(),
                            ),
                            (refresh.clone(), (x.refresh as f64 / 1e3).into()),
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
    let name = ctx.intern(":name");
    let scale = ctx.intern(":scale");
    let resolution = ctx.intern(":resolution");
    let pos_x = ctx.intern(":pos-x");
    let pos_y = ctx.intern(":pos-y");

    let args = rest.clone();

    let tgt_name: Option<String> = plist_get(args.clone(), &name)?.try_into()?;
    let tgt_scale: Option<f64> = plist_get(args.clone(), &scale)?.try_into()?;
    let tgt_resolution: Option<String> = plist_get(args.clone(), &resolution)?.try_into()?;
    let tgt_pos_x: Option<i64> = plist_get(args.clone(), &pos_x)?.try_into()?;
    let tgt_pos_y: Option<i64> = plist_get(args.clone(), &pos_y)?.try_into()?;

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

    let mut cmd = format!("output {}", tgt_name.unwrap());

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

    get_outputs(ctx)
}
