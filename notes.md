# LiveView Rust

Template ideas:

* Static parts
* Dynamic parts

A template is a list of static and dynamic things:

1. Static "Hello "
2. Dynamic "{{ name }}"
3. Static "!"

But we also need to think about constructs like for loops.

* All the things in the list are a single dynamic slot
* You elements with the id attribute and let the client fix things up

Will go with former for now since its much simpler.

We may also want to nest templates, but this is something for another time.

## Templates

I think I will hand roll my own template parser.

In general, I like to use plain HTML for the HTML bits, but then there will be a limited syntax for outputting content.
The syntax should be similar to Rust and should be very easy to translate over.

To start, we will just allow inserting data. We will use the EEX/Ruby type thing where @ means the variable comes from the "assigns".
This is an important thing to have so that we know how to change track from the root assigns.

Very basic example:
```
<h1><%= @page_title %></h1>
```

