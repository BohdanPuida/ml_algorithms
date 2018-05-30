library(keras)
library(tensorflow)
library(dummies)
library(reticulate)
library(plotly)

mnist <- dataset_mnist()

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

history <- model %>% fit(x_train, y_train, epochs = 30, batch_size = 128, validation_split = 0.2)
ggplotly(plot(history))
model %>% evaluate(x_test, y_test)


model %>% predict_classes(x_test)

#Split-apply-combine attempts
library(dplyr)
library(purrr)
library(lambda.r)
library(ggvis)
mtcars %>% 
  ggvis(~mpg, ~disp, size = ~vs) %>% 
  layer_points


iris %>% 
  ggvis(~Petal.Length, ~Petal.Width) %>% 
  layer_points %>% 
  layer_smooths(stroke := 'green') 
library(plotly)
p <- ggplot(p)
ggplotly(p)
t <- seq(0, 2 * pi, length = 20)
df <- data.frame(x = sin(t), y = cos(t))
df %>% ggvis(~x, ~y) %>% layer_paths()
menagerie %>% ggvis(~predator, ~toothed) %>% layer_smooths %>% layer_points
names(menagerie)
mtcars %>% 
  ggvis(
    ~wt, 
    ~mpg,
      size := input_slider(10, 100),
        opacity := input_slider(0, 1)
  ) %>% 
  layer_points
keys_s <- left_right(10, 1000, step = 50)
mtcars %>% ggvis(~wt, ~mpg, size := keys_s, opacity := 0.5) %>% layer_points()

mtcars %>% ggvis(~wt, ~mpg) %>% 
  layer_points() %>% 
  add_tooltip(function(df) df$wt)


#findS
findS(data, type) %::% data.frame : character : matrix
findS(data, type) %as% { 
  data %>% 
    split(.[type]) %>% 
    map(~map(., ~ifelse(length(unique(.)) > 1, '?', .))) %>% 
    do.call('rbind', .)
}
findS(menagerie, 'type')

#apply 
app_findS(data, type) %::% data.frame : character : matrix
app_findS(data, type) %as% {
  data %>% 
    split(.[type]) %>% 
    lapply(., function(x) 
      apply(x, 2, function(y) 
      ifelse(length(unique(y)) > 1, '?', y))) %>% 
    do.call('rbind', .) 
}
app_findS(menagerie, 'type')

menagerie %>% split(.$type) %>% 
  map( ~map(., print))




menagerie %>% 
  split(.$type) %>% 
  map(~map(., ~ifelse(length(unique(.)) > 1, '?', .))) %>% 
  map_dfc(unlist) %>% 
  t  -> findS
colnames(findS) <- names(menagerie)
findS

menagerie %>% 
  split(.$type) %>% 
  lapply(function(x) 
    apply(x, 2, function(y) 
      ifelse(length(unique(y)) > 1, '?', y))) %>% 
  do.call('rbind', .) 
a <- 3 + 4i
b <- 8 - 7i
a - b
sqrt(b)
menagerie %>% split(.$type) %>% 
  map(~map(., ~ifelse(length(unique(.)), '?', .))) %>%
  do.call('rbind', .)

names(menagerie)
RGui
menagerie %>% group_by(type) %>% nest() -> a
map(a, ~map(., ~length(.)))
