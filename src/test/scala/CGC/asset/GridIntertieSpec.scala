package CGC.asset

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest._

class GridIntertieSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("GridIntertieSpec"))

  "A Grid Intertie actor" should {
    "respond with its status when quried" in {
      val probe = TestProbe()
      val gridActor = system.actorOf(GridIntertie.props)

      gridActor.tell(GridIntertie.ReqGetStatus(requestId = 42), probe.ref)
      val response = probe.expectMsgType[GridIntertie.RespGetStatus]
      response.requestId should ===(42)
    }

    "initialize in the offline state" in {
      val probe = TestProbe()
      val gridActor = system.actorOf(GridIntertie.props)

      gridActor.tell(GridIntertie.ReqGetStatus(requestId = 0), probe.ref)
      val response = probe.expectMsgType[GridIntertie.RespGetStatus]
      response.requestId should ===(0)
      response.status should ===(GridIntertie.Status(online = false, kW = 0.0, kVar = 0.0))
    }

    "update its status when commanded" in {
      val probe = TestProbe()
      val gridActor = system.actorOf(GridIntertie.props)

      gridActor.tell(GridIntertie.ReqSetStatus(requestId = 0,
        GridIntertie.Status(online = false, kW = 102.7, kVar = 88.5)), probe.ref)
      val response1 = probe.expectMsgType[GridIntertie.RespSetStatus]
      response1.requestId should ===(0)

      gridActor.tell(GridIntertie.ReqGetStatus(requestId = 1), probe.ref)
      val response2 = probe.expectMsgType[GridIntertie.RespGetStatus]
      response2.requestId should ===(1)
      response2.status should ===(GridIntertie.Status(online = false, kW = 102.7, kVar = 88.5))
    }

    "transition to the GridConnectRequest state if requested" in {
      val probe = TestProbe()
      val gridActor = system.actorOf(GridIntertie.props)

      gridActor.tell(GridIntertie.ReqGridConnected(requestId = 0), probe.ref)
      val response1 = probe.expectMsgType[GridIntertie.RespGridStateCommand]
      response1.requestId should ===(0)

      gridActor.tell(GridIntertie.ReqState(requestId = 1), probe.ref)
      val response2 = probe.expectMsgType[GridIntertie.RespState]
      response2.requestId should ===(1)
      response2.state should ===(GridIntertie.GridConnectRequested)
    }

    "transition to the GridConnected state if criteria is met" in {
      val probe = TestProbe()
      val gridActor = system.actorOf(GridIntertie.props)

      gridActor.tell(GridIntertie.ReqGridConnected(requestId = 1), probe.ref)
      val response1 = probe.expectMsgType[GridIntertie.RespGridStateCommand]
      response1.requestId should ===(1)

      gridActor.tell(GridIntertie.ReqSetStatus(requestId = 2,
        GridIntertie.Status(online = true, kW = 102.7, kVar = 88.5)), probe.ref)
      val response2 = probe.expectMsgType[GridIntertie.RespSetStatus]
      response2.requestId should ===(2)

      gridActor.tell(GridIntertie.ReqState(requestId = 3), probe.ref)
      val response3 = probe.expectMsgType[GridIntertie.RespState]
      response3.requestId should ===(3)
      response3.state should ===( GridIntertie.GridConnected )
    }

    "transition to the GridDisconnectRequested state if online and disconnect is requested" in {
      val probe = TestProbe()
      val gridActor = system.actorOf(GridIntertie.props)

      // set status to force grid into GridConnected state
      gridActor.tell(GridIntertie.ReqSetStatus(requestId = 1,
        GridIntertie.Status(online = true, kW = 102.7, kVar = 88.5)), probe.ref)
      val response1 = probe.expectMsgType[GridIntertie.RespSetStatus]
      response1.requestId should ===(1)

      gridActor.tell(GridIntertie.ReqState(requestId = 2), probe.ref)
      val response2 = probe.expectMsgType[GridIntertie.RespState]
      response2.requestId should ===(2)
      response2.state should ===(GridIntertie.GridConnected)

      gridActor.tell(GridIntertie.ReqGridDisconnected(requestId = 3), probe.ref)
      val response3 = probe.expectMsgType[GridIntertie.RespGridStateCommand]
      response3.requestId should ===(3)

      gridActor.tell(GridIntertie.ReqState(requestId = 4), probe.ref)
      val response4 = probe.expectMsgType[GridIntertie.RespState]
      response4.requestId should ===(4)
      response4.state should ===(GridIntertie.GridDisconnectRequested)
    }

    "transition to the GridDisconnected state if online and disconnect criteria is met" in {
      val probe = TestProbe()
      val gridActor = system.actorOf(GridIntertie.props)

      gridActor.tell(GridIntertie.ReqSetStatus(requestId = 1,
        GridIntertie.Status(online = true, kW = 102.7, kVar = 88.5)), probe.ref)
      val response1 = probe.expectMsgType[GridIntertie.RespSetStatus]
      response1.requestId should ===(1)

      gridActor.tell(GridIntertie.ReqState(requestId = 2), probe.ref)
      val response2 = probe.expectMsgType[GridIntertie.RespState]
      response2.requestId should ===(2)
      response2.state should ===(GridIntertie.GridConnected)

      gridActor.tell(GridIntertie.ReqSetStatus(requestId = 3,
        GridIntertie.Status(online = false, kW = 102.7, kVar = 88.5)), probe.ref)
      val response3 = probe.expectMsgType[GridIntertie.RespSetStatus]
      response3.requestId should ===(3)

      gridActor.tell(GridIntertie.ReqState(requestId = 4), probe.ref)
      val response4 = probe.expectMsgType[GridIntertie.RespState]
      response4.requestId should ===(4)
      response4.state should ===(GridIntertie.GridDisconnected)
    }
  }
}
