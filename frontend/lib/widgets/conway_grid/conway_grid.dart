import 'package:flutter/material.dart';
import 'cell.dart';
import 'frame.dart';

export 'frame.dart';

class ConwayGrid extends StatelessWidget {
  final ConwayFrame frame;
  final ValueSetter<Offset>? onTap;

  const ConwayGrid({Key? key, required this.frame, this.onTap})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Column(
        mainAxisAlignment: MainAxisAlignment.center,
        crossAxisAlignment: CrossAxisAlignment.center,
        children: <Widget>[
          for (int y = frame.minY; y <= frame.maxY; y++)
            Row(
                mainAxisAlignment: MainAxisAlignment.center,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: <Widget>[
                  for (int x = frame.minX; x <= frame.maxX; x++)
                    GestureDetector(
                        onTap: () {
                          onTap?.call(Offset(x.toDouble(), y.toDouble()));
                        },
                        child: ConwayCell(alive: frame.get(x, y)))
                ])
        ]);
  }
}
