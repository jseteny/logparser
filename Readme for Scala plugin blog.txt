
http://blog.jetbrains.com/scala/


Scala Plugin 1.2 RC Introduces Ahead-of-time Code Completion
Posted on December 9, 2014 by Pavel Fatin

With the just published Scala Plugin 1.2 RC build, we’re happy to pioneer a new kind of coding
assistance, which we like to call “ahead of time” (AOT) code completion.

Have you ever noticed that unlike Java, in Scala we have to spend more time actually typing all
the parameter and variable names? In Java we can always do this:

    automatically complete a type reference,
    then automatically complete a corresponding parameter or variable name.

Which is very quick and really convenient. Unfortunately, this won’t do in Scala directly because
of these reasons:

    order of variable type and name is reversed and,
    type annotation is optional and therefore may never exist.

So everyone programming Scala was completely denied all of this completion coolness… until now.
We’ve changed the rules: now it’s possible to automatically complete both the name and the type
before actually adding a type reference (and that is why we call it “ahead of time” completion):

<<picture>>

...
