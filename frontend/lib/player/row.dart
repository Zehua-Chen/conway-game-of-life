import 'package:flutter/widgets.dart';
import 'cell.dart';

class ConwayRow extends StatelessWidget {
  final int count;

  const ConwayRow({Key? key, this.count = 1}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Row(
        children: <Widget>[for (int i = 0; i < count; i++) ConwayCell()],
        mainAxisAlignment: MainAxisAlignment.center,
        crossAxisAlignment: CrossAxisAlignment.center);
  }
}
