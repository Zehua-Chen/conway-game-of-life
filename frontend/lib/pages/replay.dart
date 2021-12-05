import 'package:flutter/material.dart';
import 'package:conway/widgets/conway_grid/conway_grid.dart';
import 'package:conway/widgets/conway_player/conway_player.dart';

class Replay extends StatefulWidget {
  const Replay({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ReplayState();
}

class _ReplayState extends State<Replay> {
  int _frame = 0;
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

  void _nextPage() {
    setState(() {
      _frame += 1;
      _frame = _frame % _frames.length;
    });
  }

  void _playingToggle() {
    setState(() {
      _playing = !_playing;
    });
  }

  @override
  Widget build(BuildContext context) {
    return ConwayPlayer(
        frames: _frames,
        frame: _frame,
        playing: _playing,
        onNextPage: _nextPage,
        onPlayingToggle: _playingToggle);
  }
}
