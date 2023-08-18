# slippy
A lisp configuration tool for sway window manager

## Installation

This has to be compiled from source, so you need the rust toolchain installed.  Not tested on rust versions older than 1.70.

Make a local clone of this repo, then run:

        make install

and that should install the `slippy` binary to `~/.local/bin` (or to whatever location this command returns: `systemd-path user-binaries`).

And add these lines to your sway config file:

        # run at startup
        exec bash -c "sleep 1; /home/<username>/.local/bin/slippy 2>&1 >> .var/slippy.log"
        # reload slippy config
        bindsym $mod+Shift+s exec bash -c "killall slippy; /home/<username>/.local/bin/slippy 2>&1 >> .var/slippy.log"


## Usage

Slippy looks for a config file in `~/.config/slippy/config.lisp`.  There's a `example-config.lisp` file in this repo that can be used as a starting point.

Slippy uses [`tulisp`](https://github.com/shsms/tulisp), which is a lisp interpreter that implements a small subset of Emacs lisp.  All functions exposed by `tulisp` can be used in slippy's config file, and a list is available at: <https://docs.rs/tulisp/latest/tulisp/builtin/index.html>

In addition to `tulisp` features, slippy currently provides these methods:


| name        | description                                                      | parameters                                                                                                                   | returns                                                      |
|-------------|------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| transitions | change transparency of windows as they become active or inactive | (1) duration for each transition, in ms. (2) opacity for active windows, 0.0 to 1.0. (3) opacity for inactive windows        | nil                                                          |
| get-outputs | return all outputs known to sway                                 | none                                                                                                                         | a list of plists, one plist per output                       |
| set-output  | set output parameters                                            | accepts keyword arguments (aka a plist) with the following keywords:  `:name`, `:scale`, `:resolution`,`:pos-x` and `:pos-y` | a plist for the output that was updated, with latest values. |


