module Main where


import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


main = do
  -- Initialize OpenGL via GLUT
  (progname, _) <- getArgsAndInitialize
  -- Create the output window
  createWindow progname
  -- Every time the window needs to be updated, call the display function
  displayCallback $= display
  -- Let GLUT handle the window events, calling the displayCallback as fast as it can
  mainLoop

display :: IO ()
display = do
  -- Clear the screen with the default clear color (black)
  clear [ColorBuffer]
  -- Render a line from the bottom left to the top right
  renderPrimitive Lines $ do
    vertex $ (Vertex3 (-1) (-1)  0 :: Vertex3 GLfloat)
    vertex $ (Vertex3   1    1   0 :: Vertex3 GLfloat)
  -- Send all of the drawing commands to the OpenGL server
  flush
