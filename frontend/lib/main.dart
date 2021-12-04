import 'package:flutter/material.dart';
import 'player/player.dart';
import 'schema/schema.dart' as schema;
import 'dart:convert';

void main() {
  final story = schema.Story([
    schema.Page([schema.LivingCell(1, 2), schema.LivingCell(1, 10)])
  ]);

  print(jsonEncode(story.toJson()));

  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
        title: 'Conway Game of Life',
        theme: ThemeData(primarySwatch: Colors.deepPurple),
        home: const MyStatefulWidget());
  }
}

class MyStatefulWidget extends StatefulWidget {
  const MyStatefulWidget({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _MyStatefulWidgetState();
}

class _MyStatefulWidgetState extends State<MyStatefulWidget> {
  int _selectedIndex = 1;

  static Route<Object?> _dialogBuilder(
      BuildContext context, Object? arguments) {
    return DialogRoute<void>(
        context: context,
        builder: (BuildContext context) {
          return const AboutDialog(
              applicationName: 'Conway Game of Life',
              applicationVersion: '1.0.0');
        });
  }

  String _title() {
    switch (_selectedIndex) {
      case 0:
        return 'Create Conway Game of Life';
      case 1:
        return 'Replay Conway Game of Life';
      default:
        throw Error();
    }
  }

  Widget _body() {
    switch (_selectedIndex) {
      case 0:
        return const Placeholder();
      case 1:
        return ConwayPlayer();
      default:
        return const Placeholder();
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
            // Here we take the value from the MyHomePage object that was created by
            // the App.build method, and use it to set our appbar title.
            title: Text(_title()),
            actions: <Widget>[
              IconButton(
                  icon: const Icon(Icons.info),
                  onPressed: () {
                    Navigator.of(context).restorablePush(_dialogBuilder);
                  })
            ]),
        body: Row(
          children: [
            NavigationRail(
              selectedIndex: _selectedIndex,
              labelType: NavigationRailLabelType.selected,
              onDestinationSelected: (int index) {
                setState(() {
                  _selectedIndex = index;
                });
              },
              destinations: const [
                NavigationRailDestination(
                    icon: Icon(Icons.create), label: Text('Create')),
                NavigationRailDestination(
                    icon: Icon(Icons.replay), label: Text('Replay')),
              ],
            ),
            Expanded(child: Center(child: _body()))
          ],
        ));
  }
}
