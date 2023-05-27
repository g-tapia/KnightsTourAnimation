# Visual demonstration

https://github.com/g-tapia/KnightsTourAnimation/assets/78235399/00dbee7d-86ed-48ee-af57-c5bd4165a8ef



## My Coding Journey: Creating the Knight's Tour Animation
Have you ever thought about mixing hobbies? Well, that's exactly what I did. I've always had a soft spot for chess, and as someone who loves coding, I found a way to blend these two together. That's how I ended up tackling the Knight's Tour problem - it was the perfect way to mix code and chess.

Here's the fun part: the Knight's Tour problem is all about moving a knight across a chessboard, making sure it visits each square only once. It's like a puzzle - a fun, challenging, sometimes head-scratching puzzle. And, to make it even more interesting, I used a smart strategy called Warnsdorff's rule. This rule says that the knight should always move to a square that has the fewest onward moves, so it doesn't get stuck too soon. Pretty cool, right?

But, what's a knight without a little bit of flair? I decided to use the Green Knight from "Sir Gawain and the Green Knight" as the image for my chess knight. It's an Arthurian legend, where the Green Knight was one of Arthur's greatest champions. Plus, the colors fit perfectly with a tournament chessboard. If you want to know more about the Green Knight, check out the link below.

Link: [Green Knight](https://en.wikipedia.org/wiki/Green_Knight)

As the knight started its journey, the image of the Green Knight showed up on the squares it visited. If I had to backtrack and change the knight's route, the square would turn back to its original color, making the Green Knight's image disappear. I had a lot of fun tweaking images and pixels to map out the knight's path - it was like watching the puzzle solve itself.

In a nutshell, this project was a whole lot of fun. It was a great way to challenge my coding skills, but more importantly, it let me bring together my love for chess and coding. I got to play around with Haskell, mess around with images, and solve a pretty neat problem. The best part? Seeing the knight making its way across the board, leaving behind a trail of Green Knight images.

## Some Lessons Learned

This was a great Haskell project - I got to visualize the solution to the Knight's Tour problem with the Gloss library and learn some cool new things.

One of the biggest lessons I learned was how to play around with images to fit into cells on a chessboard. I've never done something like this with Haskell before, so it was a new and fun challenge. I had to use the `cropImageToCellPosition` and `scaleImageToFitCell` functions to get the images just right, and I had to devise mathematical formulas so that I can map each of the regular dimensions of the board to the grid system in gloss.

The `scaleImageToFitCell` function allowed me to take a big image and shrink it down to fit a cell on the chessboard. After that, I had to take this image and cut it into a smaller square that would fit into a cell on the chessboard. This is where the `cropImageToCellPosition` function came in handy. With these two functions, I was able to fit the Green Knight into every single cell on the chessboard.

This project really showed me how versatile Haskell is. It's not just a functional programming language, but a tool that can be used for a whole bunch of things, including graphic elements and animations. I can't wait to apply these skills to my next project!
