# Visual demonstration

This repository features my implementation of the Knights' Tour animation, a classic chess problem where the knight must visit every square on the board exactly once. I added a unique twist by using the Green Knight from "Sir Gawain and the Green Knight" as the image for the visited squares. As the knight searches for a path, it paints each visited square. Before watching my animation that I recorded below, please watch [this video](https://www.youtube.com/watch?v=ab_dY3dZFHM) to understand how the algorithm works.

Another note before watching the video: If you click full screen, you can see the calculation of the number of moves available per square, giving you an idea of how the algorithm works. Imagine it like this: it's your turn, and you are about to move the knight (the only piece on the board to solve the knights' tour problem). You look at the available positions from where your knight is at and notice that there are two options on the board. You think, "If I move the knight to the first square, I will have 5 different moves to pick from on my next turn. However, if I move the knight to the second position, I will only have two moves that I can make with the knight on my next turn." With this algorithm, you will always choose the position with the minimum number of moves, so you would select position two. The goal is to keep picking the squares with the least number of moves until you solve the knights' tour problem. However, in this case, you keep going back and trying every possible move you could have made in the past until you find the correct sequence that will get you to solve this problem.

https://github.com/g-tapia/KnightsTourAnimation/assets/78235399/00dbee7d-86ed-48ee-af57-c5bd4165a8ef

My program works for any size dimensions, though, not sure who would want to stick around for a long time to watch it unfold, especially if we have a board of size 100 x 100.





To run the code yourself, download the zip file, install Stack GHCI, run stack build to install all dependencies, and then use stack ghci to start the application. For a detailed setup guide, you can watch this video ([Getting started with haskell](https://www.youtube.com/watch?v=YNkMcNM0dJI)). 

## My Coding Journey: Creating the Knight's Tour Animation
I've always had a soft spot for chess, and as someone who loves coding, I found a way to blend these two together. That's how I ended up tackling the Knight's Tour problem - it was the perfect way to mix code and chess.

The Knight's Tour problem is all about moving a knight across a chessboard, making sure it visits each square only once. It's like a puzzle - a fun, challenging, sometimes head-scratching puzzle. And, to make it even more interesting, I used a strategy called Warnsdorff's rule. This rule says that the knight should always move to a square that has the fewest onward moves, so it doesn't get stuck too soon.

Additionally, I decided to use the Green Knight from "Sir Gawain and the Green Knight" as the image for my visited squares. The Green Knight was one of Arthur's greatest champions. Plus, the colors fit perfectly with a tournament chessboard. If you want to know more about the Green Knight, check out the link below.

Link: [Green Knight](https://en.wikipedia.org/wiki/Green_Knight)

As the knight started its journey, the image of the Green Knight showed up on the squares it visited. If I had to backtrack and change the knight's route, the square would turn back to its original color, making the Green Knight's image disappear. I had a lot of fun tweaking images and pixels to map out the knight's path, it was like watching the puzzle solve itself.

In a nutshell, this project was a whole lot of fun. It was a great way to challenge my coding skills, but more importantly, it let me bring together my love for chess and coding. I got to play around with Haskell, mess around with images, and solve a pretty neat problem. The best part was seeing the knight making its way across the board, leaving behind a trail of Green Knight images.

## Some Lessons Learned

This was a great Haskell project - I got to visualize the solution to the Knight's Tour problem with the Gloss library and learn some cool new things.

One of the biggest lessons I learned was how to play around with images to fit into cells on a chessboard. I've never done something like this with Haskell before, so it was a new and fun challenge. I had to use the `cropImageToCellPosition` and `scaleImageToFitCell` functions to get the images just right, and I had to devise mathematical formulas so that I can map each of the regular dimensions of the board to the grid system in gloss.

The `scaleImageToFitCell` function allowed me to take a big image and shrink it down to fit a cell on the chessboard. After that, I had to take this image and cut it into a smaller square that would fit into a cell on the chessboard. This is where the `cropImageToCellPosition` function came in handy. With these two functions, I was able to fit the Green Knight into every single cell on the chessboard.

This project really showed me how versatile Haskell is. It's not just a functional programming language, but a tool that can be used for a whole bunch of things, including graphic elements and animations. I can't wait to apply these skills to my next project!
