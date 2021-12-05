import 'package:flutter/material.dart';
import 'configuration.dart';
import 'package:conway/widgets/widgets.dart';

class Edit extends StatefulWidget {
  const Edit({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _EditState();
}

class _EditState extends State<Edit> {
  ConwayFrame? _frame;

  void _onTap(Offset offset) {
    setState(() {
      if (_frame == null) {
        return;
      }

      bool current = _frame!.get(offset.dx.toInt(), offset.dy.toInt());
      _frame!.set(offset.dx.toInt(), offset.dy.toInt(), !current);
    });
  }

  @override
  Widget build(BuildContext context) {
    if (_frame == null) {
      return Configuration(onCreate: (size) {
        setState(() {
          _frame = ConwayFrame.fromWH(
              width: size.width.toInt(), height: size.height.toInt());
        });
      });
    }

    return ConwayGrid(frame: _frame!, onTap: _onTap);
  }
}
