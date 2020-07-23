package task

import java.awt.Color
import java.awt.image.BufferedImage
import java.util.concurrent._
import java.io.File
import javax.imageio.ImageIO
import com.typesafe.config.{Config, ConfigFactory}

object Main extends App {

  //Loading initial values from application.conf

  val config: Config = ConfigFactory.load("application.conf")
  val inputDir = config.getString("initial-values.input-directory")
  val outputDir = config.getString("initial-values.output-directory")
  val threshold = config.getInt("initial-values.threshold")
  val numberOfThreads = config.getInt("initial-values.number-of-threads")

  /*
    Invoking method to get list of photos, then ExecutorService basing on number of threads specified in conf,
    provides parallel processing of provided images, then writes files in output directory as specified in instruction
  */
  val photoList = getListofPhotos(inputDir)
  val pool: ExecutorService = Executors.newFixedThreadPool(numberOfThreads)
  var sufix: String = ""
  for(photo <- photoList)
  {
    pool.execute(new Runnable{
      def run{
        val photoName = photo.getName
        val photoFile = ImageIO.read(photo)
        val extension = photoName.split("\\.").last
        val photoNameWithOutExt = photoName.replaceFirst("[.][^.]+$", "")
        val score = brightnessScore(photoFile)
        if(score > threshold)
        {
          sufix = "_dark_"+score+"."
        }
        else
        {
          sufix = "_bright_"+score+"."
        }
        ImageIO.write(photoFile, extension, new File(outputDir+"\\"+photoNameWithOutExt+sufix+extension))
      }
    })

  }
  pool.shutdown()

  /*
    This function returns list of photos in input directory that ends with .png and .jpg extensions
  */
  def getListofPhotos(dir: String):List[File] = {
    val extensions = List("jpg", "png")
    val inputDir = new File(dir)
    if(inputDir.exists() && inputDir.isDirectory)
    {
      inputDir.listFiles.filter(_.isFile()).filter(x => extensions.exists(e => x.getName.matches(s".*\\.$e$$"))).toList
    }
    else
    {
        List[File]()
    }
  }

  /*
      Function that calculates score from 0 - 100, where 0 - perfectly bright and 100 - perfectly dark,
      It's done by turning photos into grayscale, and then calculating mean photo pixel, the values is then shifted from 0-255 to 0-100 scale

      ///This simple algorithms proved in mine testing as the most reliable, but needs high threshold value
      ///I tried with calculating luminance from different equations, then taking median or value from histograph with the most occurences
      /// but I faced problems with example photos marked as 'dark', their scores were too high, meaning that bright elements were 'unseen'
      /// by algorithm, and the same issue can be addressed to this one as well

  */
 def brightnessScore(photo: BufferedImage):Int ={
    val width = photo.getWidth
    val height = photo.getHeight
    var sum: Double =0
    val numberOfPPixels: Long = width*height
    for(x <- 0 until width ){
      for(y <- 0 until height){
        val pixel = photo.getRGB(x,y)
        val color = new Color(pixel)
        sum = sum + (color.getBlue + color.getGreen + color.getRed)/3
      }
    }
   val meanValue = math.ceil(sum/numberOfPPixels)
   (100-math.ceil(meanValue/255*100)).toInt
  }

}
