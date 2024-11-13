/******************************************************************************
C# Structural Patterns Example - 
Example program that creates a basic C# program that demonstrates:
1. Adapter Pattern
2. Decorator Pattern
3. Composite Pattern

Adapter Pattern:
    The VideoPlayerAdapter class adapts the VideoPlayer class to implement the IMediaPlayer interface, allowing it to be used interchangeably with other IMediaPlayer objects.

Decorator Pattern:
    EnhancedMediaPlayer and SubtitleMediaPlayer are decorators that add additional functionality to the base media players without modifying their code.

Composite Pattern:
    MediaGroup acts as a composite that can group IMediaPlayer objects together and play them sequentially, demonstrating the ability to treat individual and composite objects uniformly.

Key Points for Students:
    Adapter Pattern: Useful for integrating existing code that does not match the target interface.
    Decorator Pattern: Enhances or adds new behavior to objects dynamically.
    Composite Pattern: Simplifies handling of individual objects and groups of objects by treating them in the same way.
*******************************************************************************/
using System;
using System.Collections.Generic;

namespace StructuralPatterns
{
    // Common interface for media playback
    public interface IMediaPlayer
    {
        void Play();
    }

    // Concrete implementation for playing MP3 files
    public class MP3Player : IMediaPlayer
    {
        public void Play()
        {
            Console.WriteLine("Playing MP3 file...");
        }
    }

    // Adapter pattern: Adapting an existing VideoPlayer to work with IMediaPlayer interface
    public class VideoPlayer
    {
        public void PlayVideo()
        {
            Console.WriteLine("Playing video file...");
        }
    }

    public class VideoPlayerAdapter : IMediaPlayer
    {
        private readonly VideoPlayer _videoPlayer;

        public VideoPlayerAdapter(VideoPlayer videoPlayer)
        {
            _videoPlayer = videoPlayer;
        }

        public void Play()
        {
            _videoPlayer.PlayVideo(); // Adapting the video playback method to the IMediaPlayer interface
        }
    }

    // Decorator pattern: Adding functionality to media players
    public abstract class MediaPlayerDecorator : IMediaPlayer
    {
        protected IMediaPlayer _mediaPlayer;

        public MediaPlayerDecorator(IMediaPlayer mediaPlayer)
        {
            _mediaPlayer = mediaPlayer;
        }

        public virtual void Play()
        {
            _mediaPlayer.Play();
        }
    }

    public class EnhancedMediaPlayer : MediaPlayerDecorator
    {
        public EnhancedMediaPlayer(IMediaPlayer mediaPlayer) : base(mediaPlayer) { }

        public override void Play()
        {
            base.Play();
            Console.WriteLine("Enhancing audio output with surround sound...");
        }
    }

    public class SubtitleMediaPlayer : MediaPlayerDecorator
    {
        public SubtitleMediaPlayer(IMediaPlayer mediaPlayer) : base(mediaPlayer) { }

        public override void Play()
        {
            base.Play();
            Console.WriteLine("Displaying subtitles...");
        }
    }

    // Composite pattern: Grouping media items together for batch playback
    public class MediaGroup : IMediaPlayer
    {
        private readonly List<IMediaPlayer> _mediaPlayers = new List<IMediaPlayer>();

        public void AddMedia(IMediaPlayer mediaPlayer)
        {
            _mediaPlayers.Add(mediaPlayer);
        }

        public void Play()
        {
            Console.WriteLine("Playing media group:");
            foreach (var mediaPlayer in _mediaPlayers)
            {
                mediaPlayer.Play();
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            // Individual media players
            IMediaPlayer mp3Player = new MP3Player();
            IMediaPlayer videoAdapter = new VideoPlayerAdapter(new VideoPlayer());

            // Decorated media players
            IMediaPlayer enhancedMP3Player = new EnhancedMediaPlayer(mp3Player);
            IMediaPlayer subtitleVideoPlayer = new SubtitleMediaPlayer(videoAdapter);

            // Composite pattern: Grouping media players
            MediaGroup mediaGroup = new MediaGroup();
            mediaGroup.AddMedia(enhancedMP3Player);
            mediaGroup.AddMedia(subtitleVideoPlayer);

            // Play all media in the group
            mediaGroup.Play();
        }
    }
}
