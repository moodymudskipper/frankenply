

#' Documentation Shortcuts, or frankenply loops
#'
#' see `help("Question", "utils")` for former usage. To use frankenply loops,
#'   just prefix  the arguments you want to loop on with `?~` if you want a list output,
#'   or `?~~` if you want a simplified output. You can loop on several argument but
#'   with only one prefix.
#'
#' @param e1 lhs
#' @param e2 rhs
#' @export
#' @name frankenply
`?` <- function(e1, e2){
  e1 <- substitute(e1)
  if(!is.call(e1) || !identical(e1[[1]], quote(`~`))) {
    call <- do.call(substitute, list(match.call(), list(`?` = quote(utils::`?`))))
    return(eval.parent(call))
  }
  call <- sys.call(-1)
  call <- reparse_qm_tilde(call)
  env <- sys.frame(-1)
  rlang::return_from(env, eval(call, parent.frame()))
}

reparse_qm_tilde <- function(call){
  # checks if starts with `?~`
  is_prefixed <- function(x) {
    is.call(x) &&
      identical(x[[1]], quote(`?`)) &&
      is.call(x[[2]]) &&
      identical(x[[c(2,1)]], quote(`~`))
  }
  # checks if starts with `?~~`
  is_prefixed2 <- function(x) {
    is.call(x[[c(2, 2)]]) &&
      identical(x[[c(2, 1)]], quote(`~`))
  }

  prefixed_lgl <- vapply(call, is_prefixed,logical(1))
  if(!any(prefixed_lgl))
    rlang::abort("Improper use of `?~` notation. Did you use it in an argument of a primitive?")
  prefixed_lgl2 <- vapply(call[prefixed_lgl], is_prefixed2,logical(1))

  if(any(prefixed_lgl2)){
    if(!all(prefixed_lgl2)) stop("inconsistent use of frankenply prefix")
    mapper <- quote(mapply)
    iter_args <-  lapply(call[prefixed_lgl], `[[`,c(2,2,2))
  } else {
    mapper <- quote(Map)
    iter_args <-  lapply(call[prefixed_lgl], `[[`,c(2,2))
  }

  n_iter_args    <-  length(iter_args)
  arg_nms <- allNames(call[prefixed_lgl])
  arg_nms[arg_nms == ""] <- paste0("*",1:sum(arg_nms == ""))
  arg_syms  <-  lapply(arg_nms, as.symbol)
  call[prefixed_lgl] <- arg_syms

  fun_iter_args <- setNames(replicate(n_iter_args, substitute()), arg_nms)
  fun <- as.function(c(fun_iter_args, call),envir = parent.frame())
  as.call(c(mapper,fun, iter_args))
}
