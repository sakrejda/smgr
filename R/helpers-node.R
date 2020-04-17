
#' Create a label ("tag") from a name and environment
#'
#' All components are combined, none are ignored, to create the tag.
#'
#' @param name name of object the tag is created for
#' @param env an env with object contents
#' @param sep separator to use in the tag
#'
#' @return a string tag 
#' @export
compose_tag = function(name, env, sep = default_sep()) {
  env_names = ls(envir = env)
  env = rlang::env_get_list(env, env_names)
  tag_parts = character()
  for (env_name in env_names) {
    tag_parts = c(tag_parts, env_name, sep, as.character(env[[env_name]]))
  }
  envr_tag = do.call(paste, args = c(tag_parts, list(sep = '--')))
  tag = paste0(name, sep, envr_tag)
  return(tag)
}

#' Create a numeric id from a name and environment
#'
#' All components are combined, none are ignored, to create the id.
#'
#' @param name name of object the id is created for
#' @param env an env with object contents
#'
#' @return a string id (hash)
#' @export
compose_id = function(name, env, generator = smgr::default_hash()) {
  tag = compose_tag(name, env)
  id = openssl::sha512(x = tag)
  return(id)
}

