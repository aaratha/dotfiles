import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts

PanelWindow {
  id: panel
  required property var modelData
  screen: modelData

  property bool open: false
  property var menuScreen: null
  property real anchorX: 0
  property real anchorY: 0
  property var theme
  property string font: "Hack Nerd Font"
  property string expandedSsid: ""

  signal closeRequested()

  visible: panel.open && panel.modelData === panel.menuScreen
  focusable: true
  color: "transparent"

  WlrLayershell.layer: WlrLayer.Overlay
  WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand
  WlrLayershell.namespace: "quickshell-networkmenu"
  exclusionMode: ExclusionMode.Ignore

  anchors {
    top: true
    bottom: true
    left: true
    right: true
  }

  onOpenChanged: {
    if (open) {
      NetworkService.refreshAll();
    } else {
      panel.expandedSsid = "";
      NetworkService.connectError = "";
    }
  }

  Item {
    anchors.fill: parent
    focus: panel.open
    Keys.onEscapePressed: panel.closeRequested()

    MouseArea {
      anchors.fill: parent
      onClicked: panel.closeRequested()
    }

    Rectangle {
      id: box
      x: Math.max(8, Math.min(panel.anchorX - width / 2, panel.width - width - 8))
      y: panel.anchorY + 8
      width: 320
      height: Math.min(420, contentCol.implicitHeight + 24)
      radius: 16
      color: Qt.rgba(0, 0, 0, 0.5)
      border.color: Qt.rgba(1, 1, 1, 0.1)
      border.width: 1

      Behavior on height { NumberAnimation { duration: 120; easing.type: Easing.OutCubic } }

      MouseArea {
        anchors.fill: parent
        onClicked: {}
      }

      ColumnLayout {
        id: contentCol
        anchors.fill: parent
        anchors.margins: 12
        spacing: 10

        RowLayout {
          Layout.fillWidth: true
          spacing: 8

          Text {
            text: "󰖩  Network"
            color: panel.theme.textPrimary
            font.pixelSize: 14
            font.bold: true
            font.family: panel.font
            Layout.fillWidth: true
          }

          Text {
            text: "󰑐"
            color: NetworkService.scanning ? panel.theme.accentPrimary : panel.theme.textMuted
            font.pixelSize: 14
            font.family: panel.font

            MouseArea {
              anchors.fill: parent
              anchors.margins: -6
              cursorShape: Qt.PointingHandCursor
              onClicked: NetworkService.refreshAll()
            }
          }

          Rectangle {
            Layout.preferredWidth: 36
            Layout.preferredHeight: 20
            width: 36
            height: 20
            radius: 10
            color: NetworkService.wifiEnabled ? panel.theme.accentPrimary : panel.theme.bgSurface
            border.color: panel.theme.bgBorder
            border.width: 1

            Behavior on color { ColorAnimation { duration: 150 } }

            Rectangle {
              width: 16
              height: 16
              radius: 8
              color: panel.theme.bgBase
              anchors.verticalCenter: parent.verticalCenter
              x: NetworkService.wifiEnabled ? parent.width - width - 2 : 2

              Behavior on x { NumberAnimation { duration: 150; easing.type: Easing.OutCubic } }
            }

            MouseArea {
              anchors.fill: parent
              cursorShape: Qt.PointingHandCursor
              onClicked: NetworkService.toggleWifi()
            }
          }
        }

        Text {
          visible: !NetworkService.wifiEnabled
          text: "Wi-Fi is off"
          color: panel.theme.textMuted
          font.pixelSize: 12
          font.family: panel.font
          Layout.alignment: Qt.AlignHCenter
          Layout.topMargin: 20
          Layout.bottomMargin: 20
        }

        ListView {
          Layout.fillWidth: true
          Layout.preferredHeight: Math.min(280, contentHeight)
          visible: NetworkService.wifiEnabled
          clip: true
          spacing: 2
          model: NetworkService.networks

          delegate: Column {
            id: netRow
            required property var modelData
            width: ListView.view.width

            Rectangle {
              width: parent.width
              height: 38
              radius: 8
              color: netRow.modelData.active ? panel.theme.bgSelected : (rowMouse.containsMouse ? panel.theme.bgHover : "transparent")

              Behavior on color { ColorAnimation { duration: 100 } }

              RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 10
                anchors.rightMargin: 10
                spacing: 8

                Text {
                  text: {
                    const s = netRow.modelData.signal;
                    if (s >= 80) return "󰤨";
                    if (s >= 60) return "󰤥";
                    if (s >= 40) return "󰤢";
                    if (s >= 20) return "󰤟";
                    return "󰤯";
                  }
                  color: netRow.modelData.active ? panel.theme.accentPrimary : panel.theme.textSecondary
                  font.pixelSize: 14
                  font.family: panel.font
                }

                Text {
                  text: netRow.modelData.ssid
                  color: panel.theme.textPrimary
                  font.pixelSize: 12
                  font.family: panel.font
                  elide: Text.ElideRight
                  Layout.fillWidth: true
                }

                Text {
                  visible: NetworkService.connectingSsid === netRow.modelData.ssid
                  text: "󰔟"
                  color: panel.theme.accentPrimary
                  font.pixelSize: 12
                  font.family: panel.font
                }

                Text {
                  visible: netRow.modelData.secured
                  text: "󰌾"
                  color: panel.theme.textMuted
                  font.pixelSize: 11
                  font.family: panel.font
                }

                Text {
                  visible: netRow.modelData.active
                  text: "󰄬"
                  color: panel.theme.accentGreen
                  font.pixelSize: 13
                  font.family: panel.font
                }
              }

              MouseArea {
                id: rowMouse
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: {
                  if (netRow.modelData.active) {
                    NetworkService.disconnectActive();
                  } else if (netRow.modelData.secured && !netRow.modelData.known) {
                    panel.expandedSsid = panel.expandedSsid === netRow.modelData.ssid ? "" : netRow.modelData.ssid;
                  } else {
                    NetworkService.connectTo(netRow.modelData.ssid, "");
                  }
                }
              }
            }

            Item {
              width: parent.width
              height: panel.expandedSsid === netRow.modelData.ssid ? 42 : 0
              clip: true

              Behavior on height { NumberAnimation { duration: 120; easing.type: Easing.OutCubic } }

              RowLayout {
                width: parent.width
                anchors.verticalCenter: parent.verticalCenter
                anchors.leftMargin: 10
                anchors.rightMargin: 10
                x: 10
                spacing: 6
                visible: panel.expandedSsid === netRow.modelData.ssid

                Rectangle {
                  Layout.fillWidth: true
                  Layout.preferredHeight: 28
                  height: 28
                  radius: 8
                  color: panel.theme.bgSurface
                  border.color: pwField.activeFocus ? panel.theme.accentPrimary : panel.theme.bgBorder
                  border.width: 1

                  TextInput {
                    id: pwField
                    anchors.fill: parent
                    anchors.leftMargin: 8
                    anchors.rightMargin: 8
                    verticalAlignment: TextInput.AlignVCenter
                    echoMode: TextInput.Password
                    color: panel.theme.textPrimary
                    font.pixelSize: 12
                    font.family: panel.font
                    clip: true

                    Keys.onReturnPressed: {
                      NetworkService.connectTo(netRow.modelData.ssid, text);
                      panel.expandedSsid = "";
                    }
                  }
                }

                Text {
                  text: "Connect"
                  color: panel.theme.accentPrimary
                  font.pixelSize: 11
                  font.family: panel.font

                  MouseArea {
                    anchors.fill: parent
                    anchors.margins: -6
                    cursorShape: Qt.PointingHandCursor
                    onClicked: {
                      NetworkService.connectTo(netRow.modelData.ssid, pwField.text);
                      panel.expandedSsid = "";
                    }
                  }
                }
              }
            }
          }

          Text {
            anchors.centerIn: parent
            visible: NetworkService.networks.length === 0 && !NetworkService.scanning
            text: "No networks found"
            color: panel.theme.textMuted
            font.pixelSize: 12
            font.family: panel.font
          }
        }

        Text {
          visible: NetworkService.connectError !== ""
          text: NetworkService.connectError
          color: panel.theme.accentRed
          font.pixelSize: 11
          font.family: panel.font
          wrapMode: Text.Wrap
          Layout.fillWidth: true
        }
      }
    }
  }
}
