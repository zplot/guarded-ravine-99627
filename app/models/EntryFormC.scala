package models

import scala.language.implicitConversions

object EntryFormC {


  case class EntryForm1C(

                         title: String,
                         title5fields: String,
                         label1: String,
                         subject: String,
                         project: String,
                         references: String,
                         text1: String,
                         text2: String

                       )

  case class EntryFields1C(input1: String)


}


