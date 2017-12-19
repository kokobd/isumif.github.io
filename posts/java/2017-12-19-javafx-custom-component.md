---
title: Creating Custom Components in JavaFX
---

## Motivation
Recently I'm using JavaFX to build a desktop GUI application.
It took me some time to figure out how to split my GUI into
components properly. Here is the final solution that works
elegantly for me.

## Solution
According to [Oracle's reference][javafx-fxml-reference], we prepare
a FXML file like this
```xml
<?xml version="1.0" encoding="UTF-8"?>

<?import javafx.scene.control.TextArea?>
<?import javafx.scene.layout.VBox?>

<fx:root type="VBox" fx:controller="com.github.zelinf.javademo.jfx.MyComponent"
         xmlns="http://javafx.com/javafx" xmlns:fx="http://javafx.com/fxml/1">
    <TextArea fx:id="upperText" editable="false"/>
    <TextArea fx:id="editableText" VBox.vgrow="ALWAYS"/>
</fx:root>
```
Then an corresponding class
```java
package com.github.zelinf.javademo.jfx;

import javafx.fxml.FXML;
import javafx.scene.control.TextArea;
import javafx.scene.layout.VBox;

public class MyComponent extends VBox {

    public MyComponent() {
        // A helper method, will be explained
        // in detail later.
        FXMLUtils.loadFXML(this);
    }

    @FXML
    private TextArea editableText;
    @FXML
    private TextArea upperText;

    @FXML
    private void initialize() {
        upperText.textProperty().bind(editableText.textProperty());
    }

}
```
Code outside may use `MyComponent` in Java code or in FXML files,
just like thoses components existing in standard library.

There are several things still remain to explain.

Value of `type` attribute of `<fx:root>` MUST be a super class of 
MyComponent, or you will encounter runtime error.

My IDE is Intellij IDEA. I set `fx:controller` field in FXML file,
BUT that's only to tell IDE which class the controller should be.
If you don't set that attribute, the code still works fine, though
your IDE may show errors in the FXML file.

Last thing to note: What does `FXMLUtils.loadFXML(this)` do.
```java
public static <T extends Parent> void loadFXML(T component) {
    FXMLLoader loader = new FXMLLoader();
    loader.setRoot(component);
    loader.setControllerFactory(theClass -> component);

    String fileName = component.getClass().getSimpleName() + ".fxml";
    try {
        loader.load(component.getClass().getResourceAsStream(fileName));
    } catch (IOException e) {
        throw new RuntimeException(e);
    }
}
```
Copy this method in your project, and make sure your component's FXML
file has corresponding name, it should work smoothly.

Different from code sample provided by oracle, we can't use
`loader.setController`. It will throw an runtime exception telling
you that the controller is already set, while loading the FXML file.
One workaround is to remove the `fx:controller` attribute in the FXML
file, but it will break the IDE's code checker. Instead, we use
`setControllerFactory` and return `component` as controller.

Referencing this class from other FXML files may lead SceneBuilder
to crash. This is due to SceneBuilder's classpath settings. If you
use Intellij IDEA like me, you should use the SceneBuilder instance
embedded in the IDE, and that problem get fixed automatically. If
you wish to use the standalone SceneBuilder, edit its
'app/SceneBuilder.cfg' to add path to your class files to
`app.classpath`

To summarize, you need the following steps to create and use a custom
component:

+ Create a class named `YourComponent` and put `YourComponent.fxml` in
  the same directory.
+ Make `YourComponent` extend some container class, such as
  `AnchorPane`, and set the type of `fx:root` to that container class
  in the FXML file.
+ Call `FXMLUtils.loadFXML(this)` in `YourComponent`'s constructor.
  (Assume you have copied `loadFXML`)
+ Done! You should be able to reference this component from both
  Java code and other FXML files.

[javafx-fxml-reference]: https://docs.oracle.com/javase/8/javafx/api/javafx/fxml/doc-files/introduction_to_fxml.html#custom_components
