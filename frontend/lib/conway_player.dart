import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';
import 'dart:ui';

class _Painter extends CustomPainter {
  @override
  void paint(Canvas canvas, Size size) {
    Rect rect = const Offset(0, 0) & size;
    canvas.drawRect(rect, Paint()..color = Colors.grey);
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) {
    return false;
  }
}

class ConwayPlayer extends StatefulWidget {
  const ConwayPlayer({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ConwayPlayerState();
}

class _ConwayPlayerState extends State<ConwayPlayer> {
  bool _playing = false;

  _toggle() {
    setState(() {
      _playing = !_playing;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Column(children: [
      Expanded(child: CustomPaint(painter: _Painter(), child: Container())),
      Padding(
          padding: const EdgeInsets.all(16.0),
          child: IconButton(
              icon: Icon(_playing ? Icons.pause : Icons.play_arrow),
              onPressed: _toggle))
    ]);
  }
}
