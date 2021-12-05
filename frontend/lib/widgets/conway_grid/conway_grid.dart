import 'package:flutter/material.dart';
import 'cell.dart';

class ConwayFrame {
  int width = 0;
  int height = 0;

  List<bool> cells = [];

  ConwayFrame({required this.width, required this.height, required this.cells})
      : assert(height % 2 != 0),
        assert(width % 2 != 0);

  ConwayFrame.fromNestedList(List<List<bool>> rows) {
    height = rows.length;
    assert(height % 2 != 0);

    width = rows[0].length;
    assert(width % 2 != 0);

    cells = [];

    for (List<bool> row in rows) {
      cells.addAll(row);
    }
  }

  int get _centerXOffset {
    return (width / 2).floor();
  }

  int get _centerYOffset {
    return (height / 2).floor();
  }

  int get minX {
    return -_centerXOffset;
  }

  int get minY {
    return -_centerYOffset;
  }

  int get maxX {
    return _centerXOffset;
  }

  int get maxY {
    return _centerYOffset;
  }

  bool get(int x, int y) {
    x += _centerXOffset;
    y += _centerYOffset;

    return cells[y * width + x];
  }
}

class ConwayGrid extends StatelessWidget {
  ConwayFrame frame;

  ConwayGrid({Key? key, required this.frame}) : super(key: key);

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
                    ConwayCell(alive: frame.get(x, y))
                ])
        ]);
  }
}
