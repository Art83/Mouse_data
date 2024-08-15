
# Distance
perp_dist <- function(point,origin_line,end_line) {
  m <- cbind(origin_line-end_line, point - origin_line)
  d <- abs(det(m))/sqrt(sum((origin_line-end_line)*(origin_line-end_line)))
  d
} 