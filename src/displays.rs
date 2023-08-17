use std::collections::HashMap;

use swayipc_async::Connection;
use tulisp::{lists::plist_get, Error, ErrorKind, TulispContext, TulispObject};

#[derive(Debug, Clone)]
struct DisplayConfig {
    tgt_scale: Option<f64>,
    tgt_res: Option<String>,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Displays {
    list: Vec<(String, DisplayConfig)>,
}

impl Displays {
    pub fn try_new(ctx: &mut TulispContext, rest: TulispObject) -> Result<Self, Error> {
        let mut displays = Self::default();

        let sym_scale = ctx.intern(":scale");
        let sym_res = ctx.intern(":res");

        for conf in rest.base_iter() {
            let name = conf.car()?;
            if !name.symbolp() {
                return Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Output name needs to be a symbol: {name}"),
                ));
            }
            let name = name.to_string();
            let params = conf.cdr()?;

            let tgt_scale: Option<f64> = plist_get(params.clone(), &sym_scale)?.try_into()?;
            let tgt_res: Option<String> = plist_get(params.clone(), &sym_res)?.try_into()?;

            displays
                .list
                .push((name, DisplayConfig { tgt_scale, tgt_res }));
        }

        Ok(displays)
    }

    pub async fn run(&self) -> Result<(), Error> {
        let mut conn = Connection::new().await.unwrap();
        let outputs = conn.get_outputs().await.unwrap();
        let mut op_map = HashMap::<String, i64>::new();
        for output in outputs {
            if let Some(id) = output.id {
                op_map.insert(output.name, id);
            }
        }
        let mut next_left = 0;

        for (name, cfg) in self.list.iter() {
            if op_map.get(name).is_none() {
                return Err(Error::new(
                    ErrorKind::Undefined,
                    format!("Inactive/Unknown output: {}", name),
                ));
            }

            let mut cmd = format!("output {name} pos {next_left} 0");

            if let Some(scale) = cfg.tgt_scale {
                cmd = format!("{cmd} scale {scale}")
            }

            if let Some(ref res) = cfg.tgt_res {
                cmd = format!("{cmd} res {res}")
            }

            println!("Running command: {cmd}");
            conn.run_command(cmd).await.unwrap();

            let outputs = conn.get_outputs().await.unwrap();
            for output in outputs {
                if output.name.as_str() == name {
                    next_left = output.rect.width;
                }
            }
        }
        Ok(())
    }
}
