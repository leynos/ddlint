//! External-consumer fixture for the opt-in telemetry export boundary.

use ddlint::TelemetryObserver;

fn main() {
    let _observer = TelemetryObserver::new();
}
