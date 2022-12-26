use std::time::Duration;

use futures::StreamExt;
use swayipc_async::{Connection, Event, EventType, Fallible, Node, WindowChange};
use tokio::task::JoinHandle;

#[derive(Clone, Copy, Debug)]
pub(crate) struct WindowTransition {
    duration_ms: u64,
    active_opacity: f64,
    inactive_opacity: f64,
    resolution_ms: u64,
}

impl WindowTransition {
    pub fn new(
        duration_ms: i64,
        active_opacity: f64,
        inactive_opacity: f64,
        resolution_ms: Option<i64>,
    ) -> Self {
        WindowTransition {
            duration_ms: duration_ms as u64,
            active_opacity,
            inactive_opacity,
            resolution_ms: resolution_ms.unwrap_or(20) as u64,
        }
    }

    async fn set_opacity(
        &self,
        conn: &mut Connection,
        node_id: Option<i64>,
        opacity: f64,
    ) -> Fallible<()> {
        if let Some(node_id) = node_id {
            conn.run_command(format!("[con_id={}] opacity {}", node_id, opacity))
                .await?;
        }
        Ok(())
    }

    async fn perform(self, prev_id: Option<i64>, next_id: Option<i64>) {
        let mut conn = Connection::new().await.unwrap();
        let count = self.duration_ms / self.resolution_ms;
        let res = Duration::from_millis(self.resolution_ms);
        let div = count as f64 / (self.active_opacity - self.inactive_opacity);
        for ii in 0..count {
            self.set_opacity(&mut conn, prev_id, self.active_opacity - (ii as f64) / div)
                .await
                .unwrap();
            self.set_opacity(
                &mut conn,
                next_id,
                self.inactive_opacity + (ii as f64) / div,
            )
            .await
            .unwrap();
            tokio::time::sleep(res).await;
        }
        self.set_opacity(&mut conn, prev_id, self.inactive_opacity)
            .await
            .unwrap();
        self.set_opacity(&mut conn, next_id, self.active_opacity)
            .await
            .unwrap();
    }

    fn dim_inactive(&self, root: &Node) {
        for node in &root.nodes {
            if !node.nodes.is_empty() || !node.floating_nodes.is_empty() {
                self.dim_inactive(node);
            } else if !node.focused {
                tokio::spawn(self.perform(Some(node.id), None));
            }
        }
    }

    pub async fn run(&self) {
        let mut ev_conn = Connection::new().await.unwrap();
        let mut task: Option<JoinHandle<()>> = None;
        let root = ev_conn.get_tree().await.unwrap();
        self.dim_inactive(&root);
        let mut curr = root.find_focused(|n| n.focused).map(|node| node.id);
        tokio::spawn(self.perform(None, curr));
        let mut event_stream = ev_conn.subscribe(&[EventType::Window]).await.unwrap();
        while let Some(event) = event_stream.next().await {
            if let Event::Window(w) = event.unwrap() {
                if w.change != WindowChange::New
                    && w.change != WindowChange::Close
                    && w.change != WindowChange::Focus
                {
                    continue;
                }
                if curr.unwrap_or(-1) == w.as_ref().container.id {
                    continue;
                }
                let next = Some(w.container.id);
                if let Some(t) = task {
                    if !t.is_finished() {
                        t.abort()
                    }
                }
                task = Some(tokio::spawn(self.perform(curr, next)));
                curr = next;
            } else {
                unreachable!()
            }
        }
    }
}
