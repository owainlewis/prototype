# Prototype

Prototype is a sketch for a simple polyglot format for defining and generating models for
services. It uses a very similar (but more restrictive) syntax to Google protocol buffers.

## Motivation

I got fed up with difficult schemas for modeling domain objects across many languages (Swagger etc). This is an attempt at defining a stricter domain object modelling language.

Let's define our data models

```
type Card {
  required string pan;
  optional string nickname;
}

type User {
  optional  int64   id;
  required  string  name;
  required  string  email;
  required  int32   age;
  required  boolean active;
  repeated  Card   cards;
}
```

Now we can turn these into code for any programming language

```
prototype compile service.prototype --language=ruby,python,java,objective-c
```

