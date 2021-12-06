import 'dart:math';
import 'package:conway/widgets/conway_grid/conway_grid.dart';
import 'schema.dart' as schema;

extension ConwayFrameToPageConvertion on ConwayFrame {
  schema.Page toPage() {
    final cells = <schema.LivingCell>[];

    for (int x = minX; x <= maxX; x++) {
      for (int y = minY; y <= maxY; y++) {
        if (get(x, y)) {
          cells.add(schema.LivingCell(x, y));
        }
      }
    }

    return schema.Page(cells);
  }
}

extension PageToConwayFrame on schema.Page {
  ConwayFrame toFrame() {
    if (cells.isEmpty) {
      return ConwayFrame.fromWH(width: 0, height: 0);
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

    final maxAbsX = max(maxX.abs(), minX.abs());
    final maxAbsY = max(maxY.abs(), minY.abs());

    final frame = ConwayFrame.fromWH(
        width: maxAbsX * 2 + 1, height: maxAbsY * 2 + 1, alive: false);

    for (final cell in cells) {
      frame.set(cell.x, cell.y, true);
    }

    return frame;
  }
}

extension StoryToConwayFrames on schema.Story {
  List<ConwayFrame> toFrames() {
    return pages.map((page) => page.toFrame()).toList();
  }
}
