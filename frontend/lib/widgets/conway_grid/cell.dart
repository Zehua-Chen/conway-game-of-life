import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';
import 'dart:ui';

class _Painter extends CustomPainter {
  bool alive;
  Color aliveColor;
  Color deadColor;

  _Painter(
      {required this.alive, required this.aliveColor, required this.deadColor});

  @override
  void paint(Canvas canvas, Size size) {
    final rect = const Offset(0, 0) & size;
    final rrect = RRect.fromRectAndRadius(rect, const Radius.circular(2));

    canvas.drawRRect(rrect, Paint()..color = alive ? aliveColor : deadColor);
  }

  @override
  bool shouldRepaint(covariant _Painter oldDelegate) {
    return oldDelegate.alive != alive;
  }
}

class ConwayCell extends StatelessWidget {
  final bool alive;

  const ConwayCell({Key? key, this.alive = false}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final theme = Theme.of(context);
    final aliveColor = theme.primaryColor;
    const deadColor = Colors.grey;

    return MouseRegion(
        cursor: SystemMouseCursors.click,
        child: Padding(
            padding: const EdgeInsets.all(2),
            child: CustomPaint(
                painter: _Painter(
                    alive: alive, aliveColor: aliveColor, deadColor: deadColor),
                size: const Size(17, 17))));
  }
}
