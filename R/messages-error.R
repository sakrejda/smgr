

msg_specified_id = function() {
  return("An 'id' value can never be specified directly.")
}

msg_method_requires_arg = function(method, arg) {
  msg = paste0("The method '", method, "' requires the argument '", arg, "'.")
  return(msg)
}

msg_method_disallows_arg = function(method, arg) {
  msg = paste0("The method '", method, "' does not accept the argument '", arg, "'.")
  return(msg)
}

