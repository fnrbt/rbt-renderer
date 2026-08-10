FROM mcr.microsoft.com/dotnet/sdk:10.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    libegl1-mesa-dev \
    libgles2-mesa-dev \
    libgl1-mesa-dri \
    mesa-utils \
    libfontconfig1 \
    && rm -rf /var/lib/apt/lists/*

ENV LIBGL_ALWAYS_SOFTWARE=1
ENV GALLIUM_DRIVER=llvmpipe
ENV EGL_PLATFORM=surfaceless
ENV MESA_GL_VERSION_OVERRIDE=3.3
ENV MESA_GLSL_VERSION_OVERRIDE=330

WORKDIR /src
COPY renderer/ renderer/
COPY opentype/ opentype/
COPY shaper/ shaper/
COPY bidi/ bidi/
COPY unicode/ unicode/

WORKDIR /src/renderer
RUN dotnet build -c Release

CMD ["dotnet", "run", "-c", "Release", "--", "--gl"]
