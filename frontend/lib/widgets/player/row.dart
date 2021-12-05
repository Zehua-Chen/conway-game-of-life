import 'cell.dart';
import 'package:flutter/widgets.dart';
import 'package:conway/schema/schema.dart' as schema;

class ConwayRow extends StatelessWidget {
  final double minX;
  final double maxX;
  final double y;
  final schema.Page page;

  const ConwayRow(
      {Key? key,
      required this.minX,
      required this.maxX,
      required this.y,
      required this.page})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Row(
        children: <Widget>[
          for (double x = minX; x <= maxX; x++)
            ConwayCell(alive: page.isAlive(x.toInt(), y.toInt()))
        ],
        mainAxisAlignment: MainAxisAlignment.center,
        crossAxisAlignment: CrossAxisAlignment.center);
  }
}
