import 'package:flutter/material.dart';
import 'package:conway/widgets/widgets.dart';

class Configuration extends StatefulWidget {
  final ValueSetter<Size>? onCreate;

  const Configuration({Key? key, this.onCreate}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ConfigurationState();
}

class _ConfigurationState extends State<Configuration> {
  int _width = 11;
  int _height = 11;

  final GlobalKey<FormState> _form = GlobalKey<FormState>();

  void _create() {
    widget.onCreate?.call(Size(_width.toDouble(), _height.toDouble()));
  }

  @override
  Widget build(BuildContext context) {
    return Padding(
        padding: const EdgeInsets.all(16),
        child: Form(
            key: _form,
            child: Column(children: <Widget>[
              NumberFormField(
                  value: _width,
                  label: const Text('Width'),
                  onValueChanged: (value) => setState(() => _width = value)),
              NumberFormField(
                  value: _height,
                  label: const Text('Height'),
                  onValueChanged: (value) => setState(() => _height = value)),
              Padding(
                  padding: const EdgeInsets.all(16),
                  child: ElevatedButton.icon(
                      onPressed: _create,
                      label: const Text('CREATE'),
                      icon: const Icon(Icons.check)))
            ])));
  }
}
