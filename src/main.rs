use std::path::Path;
use std::process;
use std::str;
use std::{cell::RefCell, rc::Rc};
use tulisp::{destruct_bind, tulisp_fn, Error, ErrorKind, TulispContext, TulispObject};

mod outputs;

mod window_transitions;
use window_transitions::WindowTransition;

#[derive(Default, Clone)]
struct State {
    window_transitions: Option<WindowTransition>,
}

#[derive(Default, Clone)]
struct StateWrapper {
    state: Rc<RefCell<State>>,
}

impl StateWrapper {
    fn new(ctx: &mut TulispContext) -> Self {
        let wrapper = Self::default();
        let next = wrapper.clone();
        ctx.add_special_form("transitions", move |a, b| next.transitions(a, b));
        wrapper
    }

    fn transitions(
        &self,
        ctx: &mut TulispContext,
        args: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let args = ctx.eval_each(args)?;

        destruct_bind!((duration_ms active_opacity inactive_opacity &optional resolution_ms) = args);

        let mut state = self.state.as_ref().borrow_mut();
        state.window_transitions = Some(WindowTransition::new(
            duration_ms.try_into()?,
            active_opacity.try_into()?,
            inactive_opacity.try_into()?,
            resolution_ms.try_into()?,
        ));
        Ok(TulispObject::nil())
    }
}

fn init_tulisp(ctx: &mut TulispContext) -> Result<State, Error> {
    outputs::init(ctx);

    let state = StateWrapper::new(ctx);

    let cfg_path = process::Command::new("systemd-path")
        .arg("user-configuration")
        .output()
        .map(|dir| {
            Path::new(str::from_utf8(&dir.stdout).unwrap().trim())
                .join("slippy")
                .join("config.lisp")
        })
        .map_err(|e| {
            Error::new(
                ErrorKind::Undefined,
                format!("Unable to get user-config path from systemd-path: {e}"),
            )
        })?;
    ctx.eval_file(&cfg_path.to_string_lossy().to_owned())?;

    let ret = state.state.as_ref().borrow().clone();
    Ok(ret)
}

async fn run(ctx: &mut TulispContext) -> Result<(), Error> {
    let wt = init_tulisp(ctx)?;
    if let Some(wt) = wt.window_transitions {
        wt.run().await;
    }
    Ok(())
}

#[tokio::main]
async fn main() {
    let mut ctx = TulispContext::new();

    #[tulisp_fn(add_func = "ctx", name = "string<")]
    fn string_lt(a: String, b: String) -> bool {
        a < b
    }

    if let Err(e) = run(&mut ctx).await {
        println!("{}", e.format(&ctx));
        process::exit(-1);
    }
}
