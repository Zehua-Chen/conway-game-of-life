import 'package:flutter/material.dart';
import 'package:conway/widgets/conway_player/conway_player.dart';

class Replay extends StatefulWidget {
  const Replay({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ReplayState();
}

class _ReplayState extends State<Replay> {
  bool _playing = true;

  final List<ConwayFrame> _frames = [
    ConwayFrame.fromNestedList([
      [true, false, true]
    ]),
    ConwayFrame.fromNestedList([
      [false, false, true]
    ]),
    ConwayFrame.fromNestedList([
      [false, false, false]
    ]),
  ];

  void _playingToggle() {
    setState(() {
      _playing = !_playing;
    });
  }

  @override
  Widget build(BuildContext context) {
    return ConwayPlayer(
        frames: _frames, playing: _playing, onPlayingToggle: _playingToggle);
  }
}
