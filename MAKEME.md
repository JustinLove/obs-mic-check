# OBS Mic-Check

Check that audio state and active video sources are synchronized, e.g., when "BRB" is showing, mic should be off, and vice-versa.

## Requirements

- [OBS Studio](https://obsproject.com/)
- [obs-websockets](https://github.com/Palakis/obs-websocket)
- [Elm](http://elm-lang.org/) to build from source

## Usage

Add Sources/patterns that you care about ("BRB", "Starting Soon", etc.)

Sources form and orderd list. The first visible source, determines what the microphone state should be. There is a "none of the above"/default state, which determines the desired state when none are visible.

For each source, the states visible/hidden, select whether selected audio sources must be on, must be off, don't care

## Examples

BRB
- visible, mic must be off

Starting soon
- visible, mic must be off

Stream over
- visible, mic can change from on to off, probably not on for too long

None of the above:
- mic should be on
