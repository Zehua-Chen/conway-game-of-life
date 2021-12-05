import 'dart:async';
import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';

export 'package:conway/widgets/conway_grid/conway_grid.dart';
import 'package:conway/widgets/conway_grid/conway_grid.dart';

class ConwayPlayer extends StatefulWidget {
  final List<ConwayFrame> frames;
  final int frame;
  final bool playing;

  final Duration durationPerPage;
  final VoidCallback? onNextPage;
  final VoidCallback? onPlayingToggle;

  const ConwayPlayer(
      {Key? key,
      required this.frames,
      this.frame = 0,
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
    widget.onPlayingToggle?.call();
  }

  @override
  Widget build(BuildContext context) {
    if (widget.playing) {
      _timer ??= Timer(widget.durationPerPage, _tick);
    } else {
      if (_timer != null) {
        _timer?.cancel();
        _timer = null;
      }
    }

    return Column(children: [
      Expanded(child: ConwayGrid(frame: widget.frames[widget.frame])),
      Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16),
          child: LinearProgressIndicator(
              value: (widget.frame + 1) / widget.frames.length)),
      Padding(
          padding: const EdgeInsets.all(16.0),
          child: IconButton(
              icon: Icon(_timer != null ? Icons.pause : Icons.play_arrow),
              onPressed: _toggle))
    ]);
  }
}
