import 'dart:async';
import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';

import 'package:conway/schema/schema.dart' as schema;

import 'row.dart';
import 'geometry.dart';

class ConwayPlayer extends StatefulWidget {
  final schema.Story story;
  final int page;
  final bool playing;

  final Duration durationPerPage;
  final Function? onNextPage;
  final Function? onPlayingToggle;

  const ConwayPlayer(
      {Key? key,
      required this.story,
      this.page = 0,
      this.playing = true,
      this.durationPerPage = const Duration(seconds: 1),
      this.onNextPage,
      this.onPlayingToggle})
      : super(key: key);

  @override
  State<StatefulWidget> createState() => _ConwayPlayerState();
}

class _ConwayPlayerState extends State<ConwayPlayer> {
  Timer? _timer;

  @override
  void deactivate() {
    super.deactivate();

    _timer?.cancel();
  }

  void _tick() {
    widget.onNextPage?.call();
    _timer = Timer(widget.durationPerPage, _tick);
  }

  void _toggle() {
    setState(() {
      if (_timer != null) {
        _timer?.cancel();
        _timer = null;
      } else {
        _timer = Timer(widget.durationPerPage, _tick);
      }
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
            for (double y = bounding.maxY; y >= bounding.minY; y -= 1.0)
              ConwayRow(
                  minX: bounding.minX, maxX: bounding.maxX, y: y, page: page),
          ],
              mainAxisAlignment: MainAxisAlignment.center,
              crossAxisAlignment: CrossAxisAlignment.center)),
      Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16),
          child: LinearProgressIndicator(
              value: (widget.page + 1) / widget.story.length)),
      Padding(
          padding: const EdgeInsets.all(16.0),
          child: IconButton(
              icon: Icon(_timer != null ? Icons.pause : Icons.play_arrow),
              onPressed: _toggle))
    ]);
  }
}
