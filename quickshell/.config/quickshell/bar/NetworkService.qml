pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
  id: root

  property bool wifiEnabled: false
  property bool scanning: false
  property string connectingSsid: ""
  property string connectError: ""
  property var networks: []
  property var knownConnections: []

  function refreshAll() {
    wifiStateProc.running = true;
    knownProc.running = true;
    scanning = true;
    rescanProc.running = true;
  }

  function toggleWifi() {
    wifiToggleProc.command = ["nmcli", "radio", "wifi", root.wifiEnabled ? "off" : "on"];
    wifiToggleProc.running = true;
  }

  function connectTo(ssid, password) {
    root.connectError = "";
    root.connectingSsid = ssid;
    connectProc.command = password
      ? ["nmcli", "device", "wifi", "connect", ssid, "password", password]
      : ["nmcli", "device", "wifi", "connect", ssid];
    connectProc.running = true;
  }

  function forget(ssid) {
    forgetProc.command = ["nmcli", "connection", "delete", ssid];
    forgetProc.running = true;
  }

  function disconnectActive() {
    disconnectProc.running = true;
  }

  Process {
    id: wifiStateProc
    command: ["nmcli", "-t", "-f", "WIFI", "radio"]
    running: true
    stdout: StdioCollector {
      onStreamFinished: root.wifiEnabled = text.trim().toLowerCase() === "enabled"
    }
  }

  Process {
    id: wifiToggleProc
    running: false
    onExited: root.refreshAll()
  }

  Process {
    id: knownProc
    command: ["sh", "-c", "nmcli -t -f NAME,TYPE connection show | grep ':802-11-wireless$' | cut -d: -f1"]
    running: false
    stdout: StdioCollector {
      onStreamFinished: root.knownConnections = text.trim().split("\n").filter(s => s.length > 0)
    }
  }

  Process {
    id: rescanProc
    command: ["nmcli", "dev", "wifi", "rescan"]
    running: false
    onExited: scanTimer.start()
  }

  Timer {
    id: scanTimer
    interval: 1500
    onTriggered: listProc.running = true
  }

  Process {
    id: listProc
    command: ["nmcli", "-t", "-f", "IN-USE,SSID,SIGNAL,SECURITY", "dev", "wifi", "list"]
    running: false
    stdout: StdioCollector {
      onStreamFinished: {
        const seen = {};
        for (const line of text.trim().split("\n")) {
          if (!line) continue;
          const parts = line.split(":");
          const inUse = parts[0] === "*";
          const ssid = parts[1];
          const signal = parseInt(parts[2]) || 0;
          const security = parts.slice(3).join(":");
          if (!ssid) continue;
          if (seen[ssid] && seen[ssid].signal >= signal) continue;
          seen[ssid] = {
            ssid: ssid,
            signal: signal,
            secured: security !== "" && security !== "--",
            active: inUse,
            known: root.knownConnections.includes(ssid)
          };
        }
        const list = Object.values(seen);
        list.sort((a, b) => (b.active - a.active) || (b.signal - a.signal));
        root.networks = list;
        root.scanning = false;
      }
    }
  }

  Process {
    id: connectProc
    running: false
    stdout: StdioCollector {
      onStreamFinished: {
        if (/error/i.test(text) || /fail/i.test(text)) root.connectError = text.trim();
        root.connectingSsid = "";
        root.refreshAll();
      }
    }
    stderr: StdioCollector {
      onStreamFinished: {
        if (text.trim() !== "") root.connectError = text.trim();
      }
    }
  }

  Process {
    id: forgetProc
    running: false
    onExited: root.refreshAll()
  }

  Process {
    id: disconnectProc
    command: ["sh", "-c", "nmcli -t -f DEVICE,TYPE dev status | awk -F: '$2==\"wifi\"{print $1; exit}' | xargs -r nmcli dev disconnect"]
    running: false
    onExited: root.refreshAll()
  }
}
