
mddir
rm -f
rm -R

# git add
# git commit -a -m "comment" 
# git push
# git pull


# This is a test
x <- seq(1, 20, .1)
y <- sin(x)
df <- tibble::tibble(x, y)
df
ggplot2::ggplot(aes(x, y), data = df)+
  geom_line()
