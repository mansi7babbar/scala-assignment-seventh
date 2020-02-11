package com.knoldus

case class Student(id: Long, name: String)

case class Marks(subjectId: Int, studentId: Long, marksObtained: Float)

class StudentReport {

  val students = List(Student(1, "Kunal"), Student(2, "Himanshu"), Student(3, "Geetika"), Student(4, "Anurag"), Student(5, "Kiran"),
    Student(6, "Riya"), Student(7, "Tashu"), Student(8, "Charvi"), Student(9, "Rohit"), Student(10, "Vidit"))

  val marks = List(Marks(1, 1, 95), Marks(2, 1, 75), Marks(3, 1, 100), Marks(4, 1, 60), Marks(5, 1, 80),
    Marks(1, 2, 50), Marks(2, 2, 54), Marks(3, 2, 61), Marks(4, 2, 78), Marks(5, 2, 89),
    Marks(1, 3, 43), Marks(2, 3, 56), Marks(3, 3, 65), Marks(4, 3, 38), Marks(5, 3, 71),
    Marks(1, 4, 99), Marks(2, 4, 87), Marks(3, 4, 94), Marks(4, 4, 88), Marks(5, 4, 92),
    Marks(1, 5, 33), Marks(2, 5, 25), Marks(3, 5, 22), Marks(4, 5, 31), Marks(5, 5, 20),
    Marks(1, 6, 50), Marks(2, 6, 54), Marks(3, 6, 61), Marks(4, 6, 78), Marks(5, 6, 89),
    Marks(1, 7, 43), Marks(2, 7, 56), Marks(3, 7, 65), Marks(4, 7, 38), Marks(5, 7, 71),
    Marks(1, 8, 99), Marks(2, 8, 87), Marks(3, 8, 94), Marks(4, 8, 88), Marks(5, 8, 92),
    Marks(1, 9, 33), Marks(2, 9, 25), Marks(3, 9, 22), Marks(4, 9, 31), Marks(5, 9, 20),
    Marks(1, 10, 95), Marks(2, 10, 75), Marks(3, 10, 100), Marks(4, 10, 60), Marks(5, 10, 80))

  def countNumberOfPassOrFail(subjectId: Int, percentage: Float, passOrFail: String) = {
    if (passOrFail == "pass") {
      val passStudents = for (i <- marks if i.marksObtained >= percentage) yield i.marksObtained
      passStudents.length
    }
    else {
      val failStudents = for (i <- marks if i.marksObtained < percentage) yield i.marksObtained
      failStudents.length
    }
  }

  def topOrBottomMarks(subjectId: Int, count: Int, topOrBottom: String) = {
    val studentRecord = for (i <- marks; j <- students if i.subjectId == subjectId & i.studentId == j.id) yield j.name -> i.marksObtained
    if (topOrBottom == "top") {
      studentRecord.sorted(Ordering[(String, Float)].reverse).slice(0, count)
    }
    else {
      studentRecord.sorted.slice(0, count)
    }
  }

  def calculatePercentage(student: Student) = {
    marks.filter(_.studentId == student.id).map(_.marksObtained).sum / 5
  }

  def topOrBottomPercentage(top: String, count: Int) = {
    val percentages = students.map(calculatePercentage)
    val names = students.map(_.name)
    val studentsRecord = percentages zip names
    studentsRecord.sorted(Ordering[(Float, String)].reverse).slice(0, count)
  }

  def getScholarship(percentage: Float, goodScholarship: Int, normalOrNoScholarship: Int) = {
    val percentages = students.map(calculatePercentage)
    val names = students.map(_.name)
    val goodScholarshipOutput = percentages.filter(_ >= percentage) zip names
    val normalOrNoScholarshipOutput = percentages.filter(_ < percentage) zip names
    List(goodScholarshipOutput, normalOrNoScholarshipOutput)
  }

  def getStudentsPassOrFail(pass: String, percentage: Float) = {
    val percentages = students.map(calculatePercentage)
    val names = students.map(_.name)
    if (pass == "pass") {
      percentages.filter(_ < percentage) zip names
    }
    else {
      percentages.filter(_ >= percentage) zip names
    }
  }

  def getStudentsAbovePercentage(percentage: Float) = {
    val percentages = students.map(calculatePercentage)
    val names = students.map(_.name)
    percentages.filter(_ >= percentage) zip names
  }

  def studentReportCard() = {
    val percentages = students.map(calculatePercentage)
    val names = students.map(_.name)
    names zip percentages
  }

}

object StudentReport extends App {
  val stuReport = new StudentReport
  stuReport.countNumberOfPassOrFail(2, 33, "fail")
  stuReport.topOrBottomMarks(4, 5, "top")
  stuReport.topOrBottomPercentage("bottom", 3)
  stuReport.getScholarship(85, 2000, 500)
  stuReport.getStudentsPassOrFail("fail", 40)
  stuReport.getStudentsAbovePercentage(95)
  stuReport.studentReportCard()
}

