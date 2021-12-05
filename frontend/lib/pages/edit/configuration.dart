import 'package:flutter/material.dart';

class Configuration extends StatefulWidget {
  final ValueSetter<Size>? onCreate;

  const Configuration({Key? key, this.onCreate}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ConfigurationState();
}

class _ConfigurationState extends State<Configuration> {
  final TextEditingController _width = TextEditingController(text: '10');
  final TextEditingController _height = TextEditingController(text: '10');

  void _create() {
    int width = int.tryParse(_width.text) ?? 10;
    int height = int.tryParse(_height.text) ?? 10;

    widget.onCreate?.call(Size(width.toDouble(), height.toDouble()));
  }

  @override
  Widget build(BuildContext context) {
    return Padding(
        padding: const EdgeInsets.all(16),
        child: Column(children: <Widget>[
          TextField(
              controller: _width,
              decoration: const InputDecoration(label: Text('Width'))),
          TextField(
              controller: _height,
              decoration: const InputDecoration(label: Text('Height'))),
          Padding(
              padding: const EdgeInsets.all(16),
              child: ElevatedButton.icon(
                  onPressed: _create,
                  label: const Text('CREATE'),
                  icon: const Icon(Icons.check)))
        ]));
  }
}
