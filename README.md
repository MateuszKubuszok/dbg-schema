# Debug Schema

> WIP, so not much docs, examples, anything really

Generate schema for your data types and traverse them:
 * `Schema` contains only anformation about the shape of your data
 * it can be used to traverse the whole data tree
 * this way you might need to derive only 1 type class in compile time and
   provide use cases for it later on

Example:
 * [pretty print example](dbg/visitors/ShowSchema.test.scala)
