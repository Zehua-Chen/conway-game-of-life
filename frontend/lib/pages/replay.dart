import 'package:flutter/material.dart';
import 'package:conway/widgets/player/player.dart';
import 'package:conway/schema/schema.dart' as schema;

class Replay extends StatefulWidget {
  const Replay({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ReplayState();
}

class _ReplayState extends State<Replay> {
  int _pageIndex = 0;
  bool _playing = true;

  final schema.Story _story = schema.Story([
    schema.Page([schema.LivingCell(0, 0)]),
    schema.Page([schema.LivingCell(-5, -5), schema.LivingCell(5, 5)]),
    schema.Page([schema.LivingCell(-10, -10), schema.LivingCell(10, 10)])
  ]);

  void _nextPage() {
    setState(() {
      _pageIndex += 1;
      _pageIndex = _pageIndex % _story.pages.length;
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
        story: _story,
        page: _pageIndex,
        playing: _playing,
        onNextPage: _nextPage,
        onPlayingToggle: _playingToggle);
  }
}
