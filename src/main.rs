use std::path::Path;
use std::process;
use std::str;
use std::{cell::RefCell, rc::Rc};
use tulisp::{tulisp_add_func, tulisp_fn, Error, ErrorKind, TulispContext, TulispObject};

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
        tulisp_add_func!(ctx, next.transitions, "transitions");
        wrapper
    }

    #[tulisp_fn]
    fn transitions(
        &self,
        duration_ms: i64,
        active_opacity: f64,
        inactive_opacity: f64,
        resolution_ms: Option<i64>,
    ) {
        let mut state = self.state.as_ref().borrow_mut();
        state.window_transitions = Some(WindowTransition::new(
            duration_ms,
            active_opacity,
            inactive_opacity,
            resolution_ms,
        ));
    }
}

fn init_tulisp() -> Result<State, Error> {
    let mut ctx = TulispContext::new();

    outputs::register_methods(&mut ctx);

    let state = StateWrapper::new(&mut ctx);

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

async fn run() -> Result<(), Error> {
    let wt = init_tulisp()?;
    if let Some(wt) = wt.window_transitions {
        wt.run().await;
    }
    Ok(())
}

#[tokio::main]
async fn main() {
    if let Err(e) = run().await {
        println!("{e}");
        process::exit(-1);
    }
}
