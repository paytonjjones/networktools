#' Simulated Depression Profiles
#'
#' This simulated dataset contains severity ratings for 9 symptoms of major depressive
#' disorder in 1000 individuals. Symptom ratings are  assumed to be self-reported
#' on a 100 point sliding scale.
#'
#' @docType data
#'
#' @usage depression
#'
#' @format a dataframe. Columns represent symptoms and rows represent individuals
#'
#' @keywords datasets
#'
#' @examples
#' out1 <- impact(depression)
#' summary(out1)
#' plot(out1)
#'
#' out2 <- edge.impact(depression, gamma=0.75, nodes=c("sleep_disturbance", "psychomotor_retardation"))
#' summary(out2)
#' plot(out2)
#'
#' # Visualize depression networks for "low" psychomotor retardation vs. "high" psychomotor retardation
#' par(mfrow=c(1,2))
#' qgraph::qgraph(out2$lo$psychomotor_retardation, title="Low Psychomotor Retardation")
#' qgraph::qgraph(out2$hi$psychomotor_retardation, title="High Psychomotor Retardation")
#'
#'
"depression"


#' Simulated Social Engagement Data
#'
#' This simulated dataset contains binary social engagement scores for 16 individuals. For 400 social media posts
#' on a group forum, individuals were given a score of 1 if they engaged in group conversation regarding the post,
#' and a score of 0 if they did not engage with the post.
#'
#' @docType data
#'
#' @usage social
#'
#' @format a dataframe. Columns represent individuals (nodes) and rows represent engagement in social media group conversations
#'
#' @keywords datasets
#'
#' @examples
#' out1 <- impact(social, binary.data=TRUE)
#' summary(out1)
#' plot(out1)
#'
#' out2 <- edge.impact(social, binary.data=TRUE, gamma=0.2, nodes=c("Kim", "Bob", "Dan"))
#' summary(out2)
#' plot(out2)
#'
#' # Visualize the difference in the social networks depending
#' # on whether or not Joe participated (large global strength impact)
#' par(mfrow=c(1,2))
#' qgraph::qgraph(out1$Edge$lo$Joe, title="Joe Absent")
#' qgraph::qgraph(out1$Edge$hi$Joe, title="Joe Present")
#'
#' # Visualize the difference in the social networks depending
#' # on whether or not Don participated (large network structure impact)
#' par(mfrow=c(1,2))
#' qgraph::qgraph(out1$Edge$lo$Don, title="Don Absent")
#' qgraph::qgraph(out1$Edge$hi$Don, title="Don Present")
#'
#'
"social"
