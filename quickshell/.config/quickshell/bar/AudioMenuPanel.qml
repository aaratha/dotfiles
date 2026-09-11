import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Services.Pipewire
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

  readonly property var sink: Pipewire.defaultAudioSink
  readonly property var source: Pipewire.defaultAudioSource
  readonly property var outputs: Pipewire.nodes.values.filter(n => n.isSink && !n.isStream)
  readonly property var inputs: Pipewire.nodes.values.filter(n => n.audio && !n.isSink && !n.isStream)

  signal closeRequested()

  visible: panel.open && panel.modelData === panel.menuScreen
  focusable: true
  color: "transparent"

  WlrLayershell.layer: WlrLayer.Overlay
  WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand
  WlrLayershell.namespace: "quickshell-audiomenu"
  exclusionMode: ExclusionMode.Ignore

  anchors {
    top: true
    bottom: true
    left: true
    right: true
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
      width: 300
      height: Math.min(460, contentCol.implicitHeight + 24)
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
            text: "󰕾  Volume"
            color: panel.theme.textPrimary
            font.pixelSize: 14
            font.bold: true
            font.family: panel.font
            Layout.fillWidth: true
          }

          Text {
            text: {
              if (!panel.sink || !panel.sink.audio || panel.sink.audio.muted || panel.sink.audio.volume <= 0) return "󰖁";
              if (panel.sink.audio.volume < 0.33) return "󰕿";
              if (panel.sink.audio.volume < 0.66) return "󰖀";
              return "󰕾";
            }
            color: (panel.sink && panel.sink.audio && panel.sink.audio.muted) ? panel.theme.textMuted : panel.theme.accentPrimary
            font.pixelSize: 16
            font.family: panel.font

            MouseArea {
              anchors.fill: parent
              anchors.margins: -6
              cursorShape: Qt.PointingHandCursor
              onClicked: {
                if (panel.sink && panel.sink.audio) panel.sink.audio.muted = !panel.sink.audio.muted;
              }
            }
          }
        }

        Rectangle {
          id: volumeTrack
          Layout.fillWidth: true
          Layout.preferredHeight: 8
          height: 8
          radius: 4
          color: panel.theme.bgSurface
          border.color: panel.theme.bgBorder
          border.width: 1

          Rectangle {
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            anchors.leftMargin: 2
            height: parent.height - 4
            width: Math.max(0, (parent.width - 4) * Math.max(0, Math.min(1, (panel.sink && panel.sink.audio) ? panel.sink.audio.volume : 0)))
            radius: 3
            color: panel.theme.accentPrimary
          }

          MouseArea {
            anchors.fill: parent
            anchors.margins: -6
            onPressed: mouse => setVolume(mouse.x)
            onPositionChanged: mouse => { if (pressed) setVolume(mouse.x); }

            function setVolume(x) {
              if (!panel.sink || !panel.sink.audio) return;
              const ratio = Math.max(0, Math.min(1, x / volumeTrack.width));
              panel.sink.audio.volume = ratio;
              if (ratio > 0) panel.sink.audio.muted = false;
            }
          }
        }

        Text {
          text: (panel.sink && panel.sink.audio) ? (panel.sink.audio.muted ? "Muted" : Math.round(panel.sink.audio.volume * 100) + "%") : "–"
          color: panel.theme.textSecondary
          font.pixelSize: 11
          font.family: panel.font
          Layout.alignment: Qt.AlignHCenter
        }

        Text {
          text: "OUTPUT DEVICE"
          color: panel.theme.textMuted
          font.pixelSize: 10
          font.family: panel.font
          Layout.topMargin: 4
        }

        Repeater {
          model: panel.outputs

          Rectangle {
            id: outRow
            required property var modelData
            Layout.fillWidth: true
            Layout.preferredHeight: 34
            height: 34
            radius: 8
            color: outRow.modelData === panel.sink ? panel.theme.bgSelected : (outMouse.containsMouse ? panel.theme.bgHover : "transparent")

            Behavior on color { ColorAnimation { duration: 100 } }

            RowLayout {
              anchors.fill: parent
              anchors.leftMargin: 10
              anchors.rightMargin: 10
              spacing: 8

              Text {
                text: "󰓃"
                color: outRow.modelData === panel.sink ? panel.theme.accentPrimary : panel.theme.textSecondary
                font.pixelSize: 13
                font.family: panel.font
              }

              Text {
                text: outRow.modelData.description || outRow.modelData.nickname || outRow.modelData.name
                color: panel.theme.textPrimary
                font.pixelSize: 12
                font.family: panel.font
                elide: Text.ElideRight
                Layout.fillWidth: true
              }

              Text {
                visible: outRow.modelData === panel.sink
                text: "󰄬"
                color: panel.theme.accentGreen
                font.pixelSize: 12
                font.family: panel.font
              }
            }

            MouseArea {
              id: outMouse
              anchors.fill: parent
              hoverEnabled: true
              cursorShape: Qt.PointingHandCursor
              onClicked: Pipewire.preferredDefaultAudioSink = outRow.modelData
            }
          }
        }

        Text {
          text: "INPUT DEVICE"
          color: panel.theme.textMuted
          font.pixelSize: 10
          font.family: panel.font
          Layout.topMargin: 6
          visible: panel.inputs.length > 0
        }

        Repeater {
          model: panel.inputs

          Rectangle {
            id: inRow
            required property var modelData
            Layout.fillWidth: true
            Layout.preferredHeight: 34
            height: 34
            radius: 8
            color: inRow.modelData === panel.source ? panel.theme.bgSelected : (inMouse.containsMouse ? panel.theme.bgHover : "transparent")

            Behavior on color { ColorAnimation { duration: 100 } }

            RowLayout {
              anchors.fill: parent
              anchors.leftMargin: 10
              anchors.rightMargin: 10
              spacing: 8

              Text {
                text: "󰍬"
                color: inRow.modelData === panel.source ? panel.theme.accentPrimary : panel.theme.textSecondary
                font.pixelSize: 13
                font.family: panel.font
              }

              Text {
                text: inRow.modelData.description || inRow.modelData.nickname || inRow.modelData.name
                color: panel.theme.textPrimary
                font.pixelSize: 12
                font.family: panel.font
                elide: Text.ElideRight
                Layout.fillWidth: true
              }

              Text {
                visible: inRow.modelData === panel.source
                text: "󰄬"
                color: panel.theme.accentGreen
                font.pixelSize: 12
                font.family: panel.font
              }
            }

            MouseArea {
              id: inMouse
              anchors.fill: parent
              hoverEnabled: true
              cursorShape: Qt.PointingHandCursor
              onClicked: Pipewire.preferredDefaultAudioSource = inRow.modelData
            }
          }
        }
      }
    }
  }
}
