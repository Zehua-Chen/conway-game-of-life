import 'package:conway/schema/schema.dart';
import 'dart:ui';

extension PageBoundingBox on Page {
  Rect get boundingBox {
    if (cells.isEmpty) {
      return Rect.fromCenter(center: const Offset(0, 0), width: 0, height: 0);
    }

    int maxX = cells.first.x;
    int maxY = cells.first.y;

    int minX = cells.first.x;
    int minY = cells.first.y;

    for (final cell in cells) {
      if (cell.x > maxX) {
        maxX = cell.x;
      } else if (cell.x < minX) {
        minX = cell.x;
      }

      if (cell.y > maxY) {
        maxY = cell.y;
      } else if (cell.y < minY) {
        minY = cell.y;
      }
    }

    return Rect.fromCenter(
        center: const Offset(0, 0),
        width: (maxX - minX).toDouble(),
        height: (maxY - minY).toDouble());
  }
}

extension RectMinMax on Rect {
  double get minX {
    return center.dx - size.width / 2.0;
  }

  double get minY {
    return center.dy - size.height / 2.0;
  }

  double get maxX {
    return center.dx + size.width / 2.0;
  }

  double get maxY {
    return center.dy + size.height / 2.0;
  }
}
