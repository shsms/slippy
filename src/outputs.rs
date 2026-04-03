use swayipc::Connection;

use tulisp::{AsPlist, Error, Plist, Plistable, TulispContext, TulispObject};

AsPlist! {
    struct Output {
        name: String,
        make: String,
        model: String,
        serial: String,
        active: bool,
        resolution: String,
        current_mode: String,
        refresh: f64,
        transform: String,
        scale: f64,
        pos_x<":pos-x">: i64,
        pos_y<":pos-y">: i64,
        width: i64,
        height: i64,
        modes: Vec<String>,
    }
}

AsPlist! {
    struct SetOutputParams {
        name: String,
        resolution: Option<String> {= None},
        transform: Option<String> {= None},
        scale: Option<f64> {= None},
        pos_x<":pos-x">: Option<i64> {= None},
        pos_y<":pos-y">: Option<i64> {= None},
    }
}

pub(crate) fn init(ctx: &mut TulispContext) {
    ctx.add_function("get-outputs", |ctx: &mut TulispContext| {
        get_outputs().map(|o| o.into_iter().map(|o| o.into_plist(ctx)).collect::<Vec<_>>())
    });

    ctx.add_function(
        "set-output",
        |ctx: &mut TulispContext, p: Plist<SetOutputParams>| set_output(ctx, p),
    );
}

fn get_outputs() -> Result<Vec<Output>, Error> {
    let mut conn = Connection::new()
        .map_err(|e| Error::os_error(format!("get-outputs: failed to connect to sway: {}", e)))?;
    let outputs = conn
        .get_outputs()
        .map_err(|e| Error::os_error(format!("get-outputs: failed to get outputs: {}", e)))?;

    Ok(outputs
        .into_iter()
        .map(|output| Output {
            name: output.name,
            make: output.make,
            model: output.model,
            serial: output.serial,
            active: output.active,
            resolution: output
                .current_mode
                .map(|x| format!("{}x{}", x.width, x.height))
                .unwrap_or_default(),
            current_mode: output
                .current_mode
                .map(|x| format!("{}x{}@{}", x.width, x.height, x.refresh as f64 / 1e3))
                .unwrap_or_default(),
            refresh: output
                .current_mode
                .map(|x| x.refresh as f64 / 1e3)
                .unwrap_or_default(),
            transform: output.transform.map(|x| x.into()).unwrap_or_default(),
            scale: output.scale.map(|x| x.into()).unwrap_or_default(),
            pos_x: output.rect.x as i64,
            pos_y: output.rect.y as i64,
            width: output.rect.width as i64,
            height: output.rect.height as i64,
            modes: output
                .modes
                .into_iter()
                .map(|x| format!("{}x{}@{}", x.width, x.height, x.refresh as f64 / 1e3))
                .collect(),
        })
        .collect())
}

fn set_output(
    ctx: &mut TulispContext,
    plist: Plist<SetOutputParams>,
) -> Result<TulispObject, Error> {
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

    let mut conn = Connection::new()
        .map_err(|e| Error::os_error(format!("set-output: failed to connect to sway: {}", e)))?;

    conn.run_command(&cmd)
        .map_err(|e| Error::os_error(format!("set-output: failed to run command: {}", e)))?;

    get_outputs()?
        .into_iter()
        .find(|x| x.name == plist.name)
        .map(|x| x.into_plist(ctx))
        .ok_or_else(|| {
            Error::os_error(format!(
                "set-output: failed to find output with name {}",
                plist.name
            ))
        })
}
