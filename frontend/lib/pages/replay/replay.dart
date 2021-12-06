import 'package:flutter/material.dart';
import 'package:conway/widgets/conway_player/conway_player.dart';
import 'load.dart';

class Replay extends StatefulWidget {
  const Replay({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ReplayState();
}

class _ReplayState extends State<Replay> {
  bool _playing = true;
  List<ConwayFrame>? _frames;

  void _playingToggle() {
    setState(() {
      _playing = !_playing;
    });
  }

  void _onFrameOpened(List<ConwayFrame> frames) {
    setState(() {
      _frames = frames;
    });
  }

  @override
  Widget build(BuildContext context) {
    if (_frames == null) {
      return Load(onFramesOpened: _onFrameOpened);
    }

    return ConwayPlayer(
        frames: _frames!, playing: _playing, onPlayingToggle: _playingToggle);
  }
}
