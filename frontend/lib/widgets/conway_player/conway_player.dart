import 'dart:async';
import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';

export 'package:conway/widgets/conway_grid/conway_grid.dart';
import 'package:conway/widgets/conway_grid/conway_grid.dart';

class ConwayPlayer extends StatefulWidget {
  final List<ConwayFrame> frames;
  final bool playing;
  final int initFrame;

  final Duration durationPerFrame;
  final VoidCallback? onPlayingToggle;

  const ConwayPlayer(
      {Key? key,
      required this.frames,
      this.playing = true,
      this.initFrame = 0,
      this.durationPerFrame = const Duration(seconds: 1),
      this.onPlayingToggle})
      : assert(initFrame < frames.length),
        super(key: key);

  @override
  State<StatefulWidget> createState() => _ConwayPlayerState();
}

class _ConwayPlayerState extends State<ConwayPlayer> {
  Timer? _timer;
  int _frame = 0;

  @override
  void initState() {
    super.initState();

    _frame = widget.initFrame;

    if (widget.playing) {
      _timer = Timer(widget.durationPerFrame, _tick);
    }
  }

  @override
  void deactivate() {
    super.deactivate();

    _timer?.cancel();
  }

  @override
  void didUpdateWidget(covariant ConwayPlayer oldWidget) {
    super.didUpdateWidget(oldWidget);

    if (widget.playing) {
      if (!oldWidget.playing) {
        setState(() {
          _nextFrame();
        });
      }

      _timer?.cancel();
      _timer = Timer(widget.durationPerFrame, _tick);
    } else {
      _timer?.cancel();
      _timer = null;
    }
  }

  void _tick() {
    setState(() {
      _nextFrame();
    });

    _timer = Timer(widget.durationPerFrame, _tick);
  }

  void _nextFrame() {
    _frame += 1;
    _frame %= widget.frames.length;
  }

  void _toggle() {
    widget.onPlayingToggle?.call();
  }

  @override
  Widget build(BuildContext context) {
    // print(widget.frames.length - 1);

    return Column(children: [
      Expanded(child: ConwayGrid(frame: widget.frames[_frame])),
      Padding(
          padding: const EdgeInsets.all(16),
          child: Row(children: <Widget>[
            Padding(
                padding: const EdgeInsets.symmetric(horizontal: 8),
                child: Text('$_frame')),
            Expanded(
                child: LinearProgressIndicator(
                    value: (_frame + 1) / widget.frames.length)),
            Padding(
                padding: const EdgeInsets.symmetric(horizontal: 8),
                child: Text('${widget.frames.length - 1}'))
          ])),
      Padding(
          padding: const EdgeInsets.all(16.0),
          child: IconButton(
              icon: Icon(_timer != null ? Icons.pause : Icons.play_arrow),
              onPressed: _toggle))
    ]);
  }
}
