# Prototype

Prototype is a tool for auto generating domain models. The idea is to generate domain models in multiple languages (Scala, Java, Ruby, Python) from a single typed schema.

## Status (Alpha) Compiler is working

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

enum Department {
  HR
  Admin
  Engineering
}

type Employee {
  required string first;
  required string last;
  required Department department;
}

type User {
  optional  int64   id;
  required  string  name;
  required  string  email;
  required  int32   age;
  required  boolean active;
  repeated  Card    cards;
}
```

Now we can turn these into code for any programming language

```
prototype compile service.prototype --language=ruby,python,java,objective-c
```

