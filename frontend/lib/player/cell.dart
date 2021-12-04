import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';
import 'dart:ui';

class _Painter extends CustomPainter {
  @override
  void paint(Canvas canvas, Size size) {
    final rect = const Offset(0, 0) & size;
    final rrect = RRect.fromRectAndRadius(rect, const Radius.circular(2));

    canvas.drawRRect(rrect, Paint()..color = Colors.grey);
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) {
    return false;
  }
}

class ConwayCell extends StatelessWidget {
  const ConwayCell({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Padding(
        padding: const EdgeInsets.all(2),
        child: CustomPaint(painter: _Painter(), size: const Size(10, 10)));
  }
}
