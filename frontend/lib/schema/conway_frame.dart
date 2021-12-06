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
