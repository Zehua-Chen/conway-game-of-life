import 'package:flutter/material.dart';
import 'configuration.dart';

class Edit extends StatefulWidget {
  const Edit({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _EditState();
}

class _EditState extends State<Edit> {
  Size? _size;

  @override
  Widget build(BuildContext context) {
    if (_size == null) {
      return Configuration(onCreate: (size) {
        setState(() {
          _size = size;
        });
      });
    }

    return Text('width = ${_size?.width}, height = ${_size?.height}');
  }
}
