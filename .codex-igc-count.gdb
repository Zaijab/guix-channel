set pagination off
set confirm off
set breakpoint pending on
break ezmq_bind_function
commands 1
  silent
  return
  continue
end
run
