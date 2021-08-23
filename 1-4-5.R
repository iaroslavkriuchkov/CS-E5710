library(markmyassignment)
boxes <-
  matrix(
    c(2, 5, 4, 1, 1, 3),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C"), c("red", "white"))
  )
p_red <- function(boxes) {
  probA = 0.4
  probB = 0.1
  probC = 0.5
  prob = boxes[1, 1] / (boxes[1, 1] + boxes[1, 2]) * probA + boxes[2, 1] /
    (boxes[2, 1] + boxes[2, 2]) * probB + boxes[3, 1] / (boxes[3, 1] + boxes[3, 2]) *
    probC
  return(prob)
}
p_red(boxes = boxes)

p_box <- function(boxes) {
  probA = 0.4
  probB = 0.1
  probC = 0.5
  
  boxA = (boxes[1, 1] / (boxes[1, 1] + boxes[1, 2])) * probA / p_red(boxes = boxes)
  boxB = (boxes[2, 1] / (boxes[2, 1] + boxes[2, 2])) * probB / p_red(boxes = boxes)
  boxC = (boxes[3, 1] / (boxes[3, 1] + boxes[3, 2])) * probC / p_red(boxes = boxes)
  
  return (c(boxA, boxB, boxC))
}
p_box(boxes = boxes)


p_identical_twin <- function(fraternal_prob, identical_prob) {
  boy = .5
  girl = .5
  
  identical_twin = boy * identical_prob / (boy * identical_prob + boy * boy *
                                             fraternal_prob)
  return(identical_twin)
}

p_identical_twin(1/125,1/400)

library(markmyassignment)
assignment_path <-
  paste(
    "https://github.com/avehtari/BDA_course_Aalto/",
    "blob/master/assignments/tests/assignment1.yml",
    sep = ""
  )
set_assignment(assignment_path)
mark_my_assignment()
