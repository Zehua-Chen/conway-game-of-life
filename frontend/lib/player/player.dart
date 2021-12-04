import 'dart:async';
import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';

import 'package:conway/schema/schema.dart' as schema;

import 'row.dart';
import 'geometry.dart';

class ConwayPlayer extends StatefulWidget {
  final schema.Story story;
  final int page;

  final Duration durationPerPage;
  final Function? onNextPage;

  const ConwayPlayer(
      {Key? key,
      required this.story,
      this.page = 0,
      this.durationPerPage = const Duration(seconds: 1),
      this.onNextPage})
      : super(key: key);

  @override
  State<StatefulWidget> createState() => _ConwayPlayerState();
}

class _ConwayPlayerState extends State<ConwayPlayer> {
  bool _playing = true;
  Timer? _timer;
  Duration? _timerDuration;

  @override
  void initState() {
    super.initState();

    _timerDuration = widget.durationPerPage;
    _timer = Timer.periodic(widget.durationPerPage, (timer) {
      if (_playing) {
        widget.onNextPage?.call();
      }
    });
  }

  @override
  void deactivate() {
    super.deactivate();

    _timer?.cancel();
  }

  _toggle() {
    setState(() {
      _playing = !_playing;
    });
  }

  @override
  Widget build(BuildContext context) {
    final page = widget.story.pages[widget.page];
    Rect bounding = page.boundingBox;

    return Column(children: [
      Expanded(
          child: Column(
              children: [
            for (double y = bounding.minY; y <= bounding.maxY; y++)
              ConwayRow(
                  minX: bounding.minX, maxX: bounding.maxX, y: y, page: page),
          ],
              mainAxisAlignment: MainAxisAlignment.center,
              crossAxisAlignment: CrossAxisAlignment.center)),
      Padding(
          padding: const EdgeInsets.all(16.0),
          child: IconButton(
              icon: Icon(_playing ? Icons.pause : Icons.play_arrow),
              onPressed: _toggle))
    ]);
  }
}
