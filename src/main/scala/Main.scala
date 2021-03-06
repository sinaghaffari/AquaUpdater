import MonadicSimplifier._
import ackcord._
import ackcord.data._
import ackcord.syntax.TextChannelSyntax
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.typesafe.config.{Config, ConfigFactory}
import org.stellar.sdk.{Asset, AssetTypeCreditAlphaNum12, AssetTypeCreditAlphaNum4, AssetTypeNative, LiquidityPoolID, LiquidityPoolParameters, Util}
import play.api.libs.json.{JsResultException, JsValue, Json, Reads}
import play.api.libs.ws.ahc.StandaloneAhcWSClient

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Future, Promise}
import scala.util.Try
import scalacache.modes.scalaFuture._
import scalacache.guava._
import scala.jdk.DurationConverters._

object Main {
  def main(args: Array[String]): Unit = {
    val config: Config = ConfigFactory.load()
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.ignore, "AckCord")
    import system.executionContext
    val ws: StandaloneAhcWSClient = StandaloneAhcWSClient()

    // This cache will contain previous reward pairs to compare to new reward pairs.
    val cache: GuavaCache[Map[Set[Asset], AquaReward]] = GuavaCache[Map[Set[Asset], AquaReward]]
    // Initialize the cache with an empty map.
    cache.put("oldRewardPairs")(Map.empty[Set[Asset], AquaReward])

    // Load config
    val token: String = config.getString("discord.token")
    val serverId: Long = config.getLong("discord.server_id")
    val channelId: Long = config.getLong("discord.channel_id")
    val supportUserId: Long = config.getLong("discord.support_user_id")
    val shouldGreet: Boolean = config.getBoolean("discord.should_greet")
    val aquaApiPollInterval: FiniteDuration = config.getDuration("aqua.poll_interval").toScala

    // Discord ClientSettings.
    val settings = ClientSettings(token, system = system)
    for {
      // Initialize the discord client with my settings
      client <- settings.createClient().?|
      // Login to discord. This will make the bot appear online.
      _ = client.login()
      // Now that the bot is online. Retrieve the channel I'd like to send messages to.
      (channel, cacheSnapshot) <- channelFromClient(client, serverId, channelId).?|
    } yield {
      if (shouldGreet) {
        client.requestsHelper.run(channel.sendMessage(
          content =
            s"""
              |**Greetings!** I'm AquaUpdater and this is a test message.
              |Once a day when the AQUA Rewards Pairs change I'll post in this channel to let you know!
              |If you would like to be notified when I post, enable all notifications for this channel by right clicking on the channel in the channel list.
              |
              |I'm a community run bot, please reach out to <@$supportUserId> if you need support or have any suggestions.
              |""".stripMargin,

        ))(cacheSnapshot)
      }
      // Repeatedly poll the AQUA API based on the poll_interval defined in application.conf
      system.scheduler.scheduleAtFixedRate(0.seconds, aquaApiPollInterval)(() => {
        (for {
          // Pull the old reward pairs from the cache
          oldRewardPairs <- cache.get("oldRewardPairs").?|(new Exception("oldRewardPairs not cached"))
          // Retrieve the new reward pairs from the AQUA API
          rewardPairs <- ws
            .url("https://reward-api.aqua.network/api/rewards/?page=1&page_size=50")
            .get()
            .map {
              case res if res.status == 200 => Right(res.body)
              case res => Left(new Exception(
                s"""
                   |Get Aqua Reward Pairs Request returned an exception
                   |  Response Code: ${res.status} ${res.statusText}
                   |  Response Body:
                   |${res.body.lines.map(l => s"  $l")}
                   |""".stripMargin))
            }
            .map(_
              .map(Json.parse)
              .map(_ \ "results")
            )
            .map(_.flatMap(_.validate[Seq[AquaReward]].asEither.left.map(JsResultException)))
            .map(_.map(_.groupBy(p => Set(p.assetA, p.assetB)).view.mapValues(_.head).toMap)).?|
          // Figure out which pairs were removed.
          removedPairs = oldRewardPairs
            .keySet
            .diff(rewardPairs.keySet)
            .map(_.toSeq.sorted.map(assetToCode).mkString("/"))
          // Figure out which pairs were added
          addedPairs = rewardPairs
            .keySet
            .diff(oldRewardPairs.keySet)
            .map(_.toSeq.sorted.map(assetToCode).mkString("/"))
          // Update the cache with the new pairs.
          // If the pairs have not changed, then do not continue.
          // If the cached pairs are empty, then do not continue. This avoids an initial message when the bot starts up.
          _ <- cache.put("oldRewardPairs")(rewardPairs).?| if (removedPairs.nonEmpty || addedPairs.nonEmpty) && oldRewardPairs.nonEmpty
        } yield {
          // This is the actual message I send to discord.
          client.requestsHelper.run(channel.sendMessage(
            content = "",
            embed = Some(OutgoingEmbed(
              title = Some("The AQUA Reward Pairs Have Changed"),
              url = Some("https://aqua.network/rewards"),
              description = Some("Current Reward Amounts: https://aqua.fetus.io"),
              fields = Seq(
                addedPairs.headOption.map(_ => EmbedField("New Pairs", addedPairs.mkString("\n"))),
                removedPairs.headOption.map(_ => EmbedField("Removed Pairs", removedPairs.mkString("\n"))),
              ).flatten
            ))
          ))(cacheSnapshot)
        }).run.onComplete(x => println(s"Result: $x"))
      })
    }
  }

  /*
   * Given a DiscordClient (connection to discord) and target server/channel.
   * It will wait until the bot is connected to the given server and will retrieve a handle to the requested channel.
   * This handle can be used to send messages to that channel.
   *
   * Note: This bot will only work in the server, and the channel defined within application.conf.
   *       It will do nothing if it joins a different server and it will never post to any other channels.
   */
  def channelFromClient(client: DiscordClient, serverId: Long, channelId: Long): Future[(TextGuildChannel, CacheSnapshot)] = {
    val promise = Promise[(TextGuildChannel, CacheSnapshot)]()
    client.onEventSideEffects { implicit c: CacheSnapshot =>
    {
      case APIMessage.GuildCreate(guild: Guild, _) if guild.id == GuildId(serverId) =>
        promise.complete(Try(guild.channels.get(GuildChannelId(channelId)).get.asInstanceOf[TextGuildChannel]).map((_, c)))
    }
    }
    promise.future
  }

  /*
   * Takes a Stellar Asset and returns its Asset Code
   */
  def assetToCode(asset: Asset): String = asset match {
    case _: AssetTypeNative => "XLM"
    case a: AssetTypeCreditAlphaNum4 => a.getCode
    case a: AssetTypeCreditAlphaNum12 => a.getCode
  }

  // Represents an AQUA Reward pair.
  case class AquaReward(assetA: Asset, assetB: Asset, daily_amm_reward: BigDecimal, daily_sdex_reward: BigDecimal,
                        daily_total_reward: BigDecimal)

  // Json reader that can convert json to an AquaReward
  implicit val aquaRewardReads: Reads[AquaReward] = (json: JsValue) => for {
    asset1_code <- (json \ "market_key" \ "asset1_code").validate[String]
    asset1_issuer <- (json \ "market_key" \ "asset1_issuer").validate[String]
    asset1 = (asset1_code, asset1_issuer) match {
      case ("XLM", "") => new AssetTypeNative()
      case _ => Asset.create(s"$asset1_code:$asset1_issuer")
    }
    asset2_code <- (json \ "market_key" \ "asset2_code").validate[String]
    asset2_issuer <- (json \ "market_key" \ "asset2_issuer").validate[String]
    asset2 = (asset2_code, asset2_issuer) match {
      case ("XLM", "") => new AssetTypeNative()
      case _ => Asset.create(s"$asset2_code:$asset2_issuer")
    }
    daily_amm_reward <- (json \ "daily_amm_reward").validate[BigDecimal]
    daily_sdex_reward <- (json \ "daily_sdex_reward").validate[BigDecimal]
    daily_total_reward <- (json \ "daily_total_reward").validate[BigDecimal]
  } yield {
    AquaReward(
      asset1,
      asset2,
      daily_amm_reward,
      daily_sdex_reward,
      daily_total_reward
    )
  }
}
