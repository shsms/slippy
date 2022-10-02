install:
	cargo build --release
	install -m 555 target/release/slippy $(shell systemd-path user-binaries)/
