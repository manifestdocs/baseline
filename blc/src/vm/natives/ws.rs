// WebSocket native functions for Baseline.
//
// The WS channels are set/cleared by `hyper_server.rs` when dispatching
// a VmWs handler. Provides `Ws.send!`, `Ws.receive!`, and `Ws.close!`
// that operate on a thread-local active WebSocket connection.

use super::{NValue, NativeError, RcStr};

#[cfg(feature = "async-server")]
use std::cell::RefCell;

#[cfg(feature = "async-server")]
use tokio::sync::mpsc;



// ---------------------------------------------------------------------------
// WebSocket message types exchanged between the VM handler and the WS task
// ---------------------------------------------------------------------------

#[cfg(feature = "async-server")]
#[derive(Debug)]
pub enum WsCommand {
    /// Send a text message to the client
    SendText(String),
    /// Close the connection gracefully
    Close,
}

#[cfg(feature = "async-server")]
#[derive(Debug)]
pub enum WsEvent {
    /// Received a text message from the client
    Text(String),
    /// Received a binary message (as raw bytes)
    Binary(Vec<u8>),
    /// Connection closed (by client or error)
    Closed(Option<String>),
}

// ---------------------------------------------------------------------------
// Thread-local WS channel (set per handler invocation)
// ---------------------------------------------------------------------------

#[cfg(feature = "async-server")]
thread_local! {
    /// Sending commands to the WS writer task
    static WS_CMD_TX: RefCell<Option<mpsc::Sender<WsCommand>>> = const { RefCell::new(None) };
    /// Receiving events from the WS reader task
    static WS_EVT_RX: RefCell<Option<mpsc::Receiver<WsEvent>>> = const { RefCell::new(None) };
}

/// Install WS channels for the current handler invocation.
#[cfg(feature = "async-server")]
pub fn set_ws_channels(
    cmd_tx: mpsc::Sender<WsCommand>,
    evt_rx: mpsc::Receiver<WsEvent>,
) {
    WS_CMD_TX.with(|cell| *cell.borrow_mut() = Some(cmd_tx));
    WS_EVT_RX.with(|cell| *cell.borrow_mut() = Some(evt_rx));
}

/// Clear WS channels after handler completes.
#[cfg(feature = "async-server")]
pub fn clear_ws_channels() {
    WS_CMD_TX.with(|cell| *cell.borrow_mut() = None);
    WS_EVT_RX.with(|cell| *cell.borrow_mut() = None);
}

// ---------------------------------------------------------------------------
// Native functions
// ---------------------------------------------------------------------------

/// Ws.send!(message: String) -> Unit
///
/// Send a text message over the active WebSocket connection.
pub fn native_ws_send(args: &[NValue]) -> Result<NValue, NativeError> {
    #[cfg(not(feature = "async-server"))]
    {
        let _ = args;
        Err(NativeError("Ws.send! requires async-server feature".into()))
    }

    #[cfg(feature = "async-server")]
    {
        let msg = match args.first().and_then(|v| v.as_string()) {
            Some(s) => s.to_string(),
            None => {
                return Err(NativeError(format!(
                    "Ws.send!: expected String, got {}",
                    args.first().map(|v| format!("{}", v)).unwrap_or_default()
                )));
            }
        };

        WS_CMD_TX.with(|cell| {
            let borrow = cell.borrow();
            match borrow.as_ref() {
                Some(tx) => {
                    // Use blocking_send since we're on the VM thread (sync context)
                    tx.blocking_send(WsCommand::SendText(msg))
                        .map_err(|_| NativeError("Ws.send!: connection closed".into()))?;
                    Ok(NValue::unit())
                }
                None => Err(NativeError(
                    "Ws.send!: no active WebSocket connection".into(),
                )),
            }
        })
    }
}

/// Ws.receive!() -> Result<String, String>
///
/// Block until a message arrives from the client.
/// Returns Ok(message) for text, Err(reason) on close/error.
pub fn native_ws_receive(args: &[NValue]) -> Result<NValue, NativeError> {
    #[cfg(not(feature = "async-server"))]
    {
        let _ = args;
        Err(NativeError("Ws.receive! requires async-server feature".into()))
    }

    #[cfg(feature = "async-server")]
    {
        let _ = args;
        WS_EVT_RX.with(|cell| {
            let mut borrow = cell.borrow_mut();
            match borrow.as_mut() {
                Some(rx) => {
                    // Use blocking_recv since we're on the VM thread (sync context)
                    match rx.blocking_recv() {
                        Some(WsEvent::Text(msg)) => Ok(NValue::enum_val(
                            "Ok".into(),
                            NValue::string(RcStr::from(msg)),
                        )),
                        Some(WsEvent::Binary(data)) => {
                            // Convert binary to string (lossy)
                            let s = String::from_utf8_lossy(&data).to_string();
                            Ok(NValue::enum_val(
                                "Ok".into(),
                                NValue::string(RcStr::from(s)),
                            ))
                        }
                        Some(WsEvent::Closed(reason)) => Ok(NValue::enum_val(
                            "Err".into(),
                            NValue::string(RcStr::from(
                                reason.unwrap_or_else(|| "connection closed".to_string()),
                            )),
                        )),
                        None => Ok(NValue::enum_val(
                            "Err".into(),
                            NValue::string(RcStr::from("connection closed")),
                        )),
                    }
                }
                None => Err(NativeError(
                    "Ws.receive!: no active WebSocket connection".into(),
                )),
            }
        })
    }
}

/// Ws.close!() -> Unit
///
/// Gracefully close the active WebSocket connection.
pub fn native_ws_close(args: &[NValue]) -> Result<NValue, NativeError> {
    #[cfg(not(feature = "async-server"))]
    {
        let _ = args;
        Err(NativeError("Ws.close! requires async-server feature".into()))
    }

    #[cfg(feature = "async-server")]
    {
        let _ = args;
        WS_CMD_TX.with(|cell| {
            let borrow = cell.borrow();
            match borrow.as_ref() {
                Some(tx) => {
                    let _ = tx.blocking_send(WsCommand::Close);
                    Ok(NValue::unit())
                }
                None => Err(NativeError(
                    "Ws.close!: no active WebSocket connection".into(),
                )),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ws_send_without_connection_errors() {
        let result = native_ws_send(&[NValue::string(RcStr::from("hello"))]);
        assert!(result.is_err());
    }

    #[test]
    fn ws_receive_without_connection_errors() {
        let result = native_ws_receive(&[]);
        assert!(result.is_err());
    }

    #[test]
    fn ws_close_without_connection_errors() {
        let result = native_ws_close(&[]);
        assert!(result.is_err());
    }

    #[test]
    fn ws_send_requires_string_arg() {
        // With async-server, should error about wrong type or no connection
        // Without async-server, should error about feature
        let result = native_ws_send(&[NValue::int(42)]);
        assert!(result.is_err());
    }
}
