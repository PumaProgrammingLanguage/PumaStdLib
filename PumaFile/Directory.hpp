#ifndef PUMA_FILE_DIRECTORY_HPP
#define PUMA_FILE_DIRECTORY_HPP

#pragma once

#include "../PumaTypes/String.hpp"

namespace Puma {
namespace File
{
    namespace Directory
    {
        // Get current working directory - normalized to forward slashes
        Types::String GetCurrentDirectory() noexcept;

        // Set current working directory - accepts a Puma String path
        bool SetCurrentDirectory(const Types::String& path) noexcept;
    } // namespace Directory
} // namespace File
} // namespace Puma

#endif // PUMA_FILE_DIRECTORY_HPP