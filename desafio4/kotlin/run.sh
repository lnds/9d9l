#!/bin/bash

printf -v var "'%s', " "$@"
var=${var%??}
gradle run -PappArgs="[$var]"
